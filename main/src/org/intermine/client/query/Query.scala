package org.intermine.client.query

import scala.collection.mutable.Stack
import scala.xml._

import org.intermine.client.model.AttributePath
import org.intermine.client.model.EndsInTable
import org.intermine.client.model.Model
import org.intermine.client.model.Path
import org.intermine.client.model.ReferencePath
import org.intermine.client.model.RootPath
import org.intermine.client.model.Table
import org.intermine.client.query.Joins._
import org.intermine.client.query.Sorting._
import org.intermine.client.query.constraint.LogicalTree._
import org.intermine.client.query.constraint._
import org.intermine.client.query.constraint.NonEmptyTree
import org.intermine.client.Service

import scalaz._
import Validation.Monad._

class Query(
    val service: Service, 
    val root: RootPath, 
    val views: Seq[AttributePath], 
    val sortOrder: Seq[Pair[AttributePath, SortDirection]], 
    val subclasses: Map[ReferencePath, Table], 
    val constraints: LogicalTree[Constraint[Path]],
    val join: Map[ReferencePath, JoinStyle]) extends Queriable  { 
  
  import Scalaz._
  
  /**
   * Creates a new query with the given columns selected for output. 
   */
  def select(views: String*): Validation[String, Query] = {
    val m = service.model
    views.map(v => if (v.startsWith(root.toString)) v else root.toString + "." + v)
         .flatMap(v => if (v.endsWith("*")) Query.star(m, v, subclasses) else Seq(v))
         .foldLeft(success[String, Seq[AttributePath]](Nil))((a, b) => a.fold(
        		 err => err.fail, 
        		 l => asPath(b).fold(e => e.fail, p => p match {
                      case x:AttributePath => success(l ++ Seq(x)); 
                      case _ => (b + " is not an attribute").fail})))
         .map(v => new Query(service, root, v, sortOrder, subclasses, constraints, join)) 
  }
  
  /**
   * Add a sort-order element, with the default direction, to the query 
   */
  def orderBy(pathString: String): Validation[String, Query] = orderBy(pathString, Sorting.ASC)
  
  /**
   * Add a sort-order element and a given direction to the query.
   */
  def orderBy(pathString: String, direction: SortDirection): Validation[String, Query] = for {
    path      <- asPath(pathString)
    orderPath <- validation[String, AttributePath](path match {
      	case x:AttributePath => Right(x); case _ => Left(path + " does not refer to an attribute")})
    fromClass <- orderPath.parent
    _         <- validation[String, String](if (selectsFrom(fromClass)) Right("ok") 
    			else Left(orderPath + "(" + fromClass + ") is not on a selected class"))
  } yield new Query(service, root, views, sortOrder ++ Seq((orderPath, direction)), subclasses, constraints, join)
  
  def orderBy(pathString: String, dir: Any): Validation[String, Query] = Sorting.values.find(v => v.toString == dir.toString.toUpperCase) match {
      case None => Failure(dir + " is not a valid direction")
      case Some(direction) => orderBy(pathString, direction)
  }
  
  /** 
   * Add a series of sort-order and direction pairs to the query.
   */
  def orderBy(soes: (String, Any)*): Validation[String, Query] = {
    val withDirections: Validation[String, Seq[(String, SortDirection)]] = soes match {
      case os: Seq[(String, SortDirection)] => Success(os)
      case os: Seq[(String, String)] => os.foldLeft(Success(Seq()):Validation[String, Seq[(String, SortDirection)]])((state, pair) => state.fold(
        e => e.fail,
        s => pair match { case (p, d) => {
        	Sorting.values.find(v => v.toString == d.toUpperCase) match {
              case None => Failure(d + " is not a valid direction")
              case Some(dir) => Success(s ++ Seq((p, dir)))
            }
        }}
      ))
      case _ => Failure("Can only handles sequences of type Seq[(String, String)], or Seq[(String, SortDirection)]")
    }
    val seed: Validation[String, Query] = Success(this)
    soes.foldLeft(seed)((v, soe) => soe match { case (p, d) => v.flatMap(q => q.orderBy(p, d))})
  }
  	
  def selectsFrom(path: Path): Boolean = views.map(v => v.parent.fold(_ => false, p => p == path)).reduce(_ || _)
  
  /**
   * Add subclass information to the query. 
   */
  def subclassing(p: String, className: String): Validation[String, Query] = for {
    path <- asPath(p)
    sc <- service.model.getTable(className)
    refPath <- validation[String, ReferencePath](path match {
      			case x:ReferencePath => Right(x); case _ => Left(path + " does not refer to a reference")})
    _       <- validation[String, String](if (sc.inheritsFrom(refPath.endTable)) Right("ok") 
    			else Left(refPath.endTable + " is not a superclass of " + sc))
  } yield {
    val newScs = subclasses.updated(refPath, sc)
    val newRoot = new RootPath(service.model, root.endTable, newScs)
    val newViews = views.foldLeft(Nil:Seq[AttributePath])((a, v) => asPath(v.toString).fold(
        e => throw new IllegalStateException("Error when remaking view: " + e),
        p => p match {
          case x:AttributePath => Seq(x) ++ a 
          case _ => throw new IllegalStateException("wrong path type")}))
    new Query(
      service, newRoot, newViews, sortOrder, 
      newScs, 
      constraints, join) 
  }
  
  def outerjoin(p: String): Validation[String, Query] = for {
    path <- asPath(p)
    refPath <- validation[String, ReferencePath](path match {
      			case x:ReferencePath => Right(x); case _ => Left(path + " does not refer to a reference")})
  } yield {
    new Query(service, root, views, sortOrder, subclasses, constraints, join.updated(refPath, Joins.OUTER))
  }
  
  def asPath(p: String): Validation[String, Path] = {
    if (p == root.toString()) return Success(root)
    val deheaded = if (p.startsWith(root.toString())) p.substring(p.indexOf(".") + 1) else p 
    val parts = deheaded.split("\\.")
    
    parts.foldLeft(success[String, Path](root))((path, part) => 
      path.fold(
          e => failure[String, Path]("Could not append " + part + ", because: " + e),
    	  s => s.append(part)))
    	  
  }
  
  def where(c: Constraint[String]): Validation[String, Query] = where(LogicalNode(c))
  
  def where(c: NonEmptyTree[Constraint[String]]): Validation[String, Query] = for {
    applied <- applyPaths(c)
  } yield {
    val newCons = constraints match {
      case EmptyTree() => applied
      case ne:NonEmptyTree[Constraint[Path]] => LogicGroup(BooleanOperator.AND, ne, applied)
    }
    new Query(service, root, views, sortOrder, subclasses, newCons, join)
  }
  
  def applyPaths(t: NonEmptyTree[Constraint[String]]): Validation[String, NonEmptyTree[Constraint[Path]]] = {
    def err[T, X]: (T => (String => Validation[String, X])) = con => (e => Failure("Error validating " + con + ": " + e)) 
    val f: (Constraint[String] => Validation[String, Constraint[Path]]) = (a) => a match {
      case AttributeConstraint(p, o, v) => asPath(p).fold(err(a), path => Success(AttributeConstraint(path, o, v)))
      case LookupConstraint(p, v, e) => asPath(p).fold(err(a), path => Success(LookupConstraint(path, v, e)))
      case LoopConstraint(p, o, v) => for (path <- asPath(p); loopPath <- asPath(v)) yield LoopConstraint(path, o, loopPath)
      case MultiConstraint(p, o, vs) => for (path <- asPath(p)) yield MultiConstraint(path, o, vs)
      case ListConstraint(p, o, v) => for (path <- asPath(p)) yield ListConstraint(path, o, v)
      case NullConstraint(p, o) => for (path <- asPath(p)) yield NullConstraint(path, o)
    }
    val validated = t map f
    def combineValidations[A](lt: NonEmptyTree[Validation[String, A]]): Validation[String, NonEmptyTree[A]] = {
      lt match {
        case LogicalNode(Failure(msg)) => Failure(msg)
        case LogicalNode(Success(v)) => Success(LogicalNode(v))
        case LogicGroup(op, l, r) => for {
          vl <- combineValidations(l)
          vr <- combineValidations(r)
        } yield LogicGroup(op, vl, vr)
      }
    }
    combineValidations(validated)
  }
  
  def asQueryParameters: Map[String, String] = Map("query" -> toXML.toString())
  
  def path = "/query/results"
  
  def toXML : scala.xml.Node = scala.xml.Utility trim <query 
           model={service.model.getName} 
           view={ views.map(_.toString()).foldLeft("")((a, b) => if (a.isEmpty()) b else a + " " + b)}
           sortOrder={
             if (sortOrder.isEmpty) 
            	 views.first.toString + " asc"
             else 
            	 sortOrder.map(_ match {case (p, d) => p + " " + d.toString.toLowerCase})
             		      .addString(new StringBuilder, " ").toString
           }
           constraintLogic={ LogicalTree.toLogicString(constraints) }
           >
           {join.flatMap(_ match {case (p, s) => Seq(<join path={p.toString} style={s.toString}/>)})}
           {subclasses.flatMap(_ match {case (p, t) => Seq(<constraint path={p.toString} type={t.name}/>)})} 
           {constraints.foldr[Seq[Elem]](Seq())((a, z) => z ++ Seq(a.toElem))}
    </query>
           
  override def toString = toXML.toString()
}

object Query {
  
  import Scalaz._
  
  val validCodes = 'A' to 'Z'
  
  def fromXML(s: Service, src: String): Validation[String, Query] = {
    val xml = XML.loadString(src)
    fromXML(s, xml)
  }
  
  def fromXML(s: Service, xml: scala.xml.Node): Validation[String, Query] = {
    // Parse out info
    val query = xml
    val views = (query \ "@view").text.split(" ")
    val so = for (soe <- (query \ "@sortOrder").text.split(" ") if !soe.isEmpty()) yield soe
    val constraintLogic = (query \ "@constraintLogic").text
    val modelName = (query \ "@model").text
    val outerjoins = for (e <- (query \ "join") if (e \ "@style").text == "OUTER") yield (e \ "@path").text
    val subclassConstraints = for (e <-(query \ "constraint") if !e.attribute("type").isEmpty) yield ((e \ "@path").text, (e\"@type").text)  
    val logicalConstraints  = for (e <-(query \ "constraint") if e.attribute("type").isEmpty) yield e
    
    // Construct the query
    val start = if (views.isEmpty) Failure("views are empty") else Success(views.first.split("\\.").first) >>= (r => s from r)
    val subclassed = subclassConstraints.foldLeft(start)((v, scc) => v >>= (q => scc match { case (p, t) => q.subclassing(p, t)}))
    val withViews  = subclassed >>= (_.select(views:_*))
    
    val withOrder  = so match {
      case Array()    => withViews
      case Array(soe) => withViews >>= (_.orderBy(soe))
      case sos if sos.size % 2 == 0 => {
        var i = (0 until sos.size).iterator
        val (evens, odds) = sos.partition(x => i.next() % 2 == 0)
        withViews >>= (_.orderBy(evens.zip(odds):_*))
      }
      case _ => Failure("Sort order has odd number of elements")
    }
    
    val joined = outerjoins.foldLeft(withOrder)((v, oj) => v >>= (_.outerjoin(oj)))
    // Parse the constraints
    var allocatedCodes: Set[Char] = Set()
    val getCode: scala.xml.Node => Validation[String, Char] = e => (e \ "@code").text.firstOption match {
      case Some(c) => {allocatedCodes += c; Success(c)}
      case None    => {
        validCodes.find(vc => !allocatedCodes.contains(vc)) match {
          case Some(c) => {allocatedCodes += c; Success(c)}
          case None => Failure("All valid codes have been allocated")
        }
      }
    }
     
    val vs = logicalConstraints.map(n => (getCode(n) >>= (code => makeConstraint(n) map (con => code -> con))))
    val constraintMap = if (vs.exists(_.isFailure))
      Failure("Errors making constraints: " + (vs.filter(_.isFailure).map(_ ||| (e => e)).mkString("; ")))
      else 
      Success(Map(vs.map(_ | (('X', null))):_*))
    
    // The constraints, all treed up
    val constraintTree = if (logicalConstraints.size == 1)
      constraintMap map (m => LogicalNode(m.values.first))
      else toTree(constraintLogic) >>=
      (t => constraintMap >>= (cm => if (cm.keys.forall(k => t.hasNode(k))) Success(t) else Failure("Logic and constraints disagree"))) >>=
      (t => constraintMap >>= (cm => Success(t map (c => cm(c)))))
    
    joined >>= (q => constraintTree >>= (tree => tree match {
      case EmptyTree() => Success(q)
      case net:NonEmptyTree[Constraint[String]] => q.where(net) 
    }))
  }
  
  def makeConstraint(node: scala.xml.Node): Validation[String, Constraint[String]] = {
    val path = (node \ "@path").text
    val op = (node \ "@op").text
    val values = (node \ "value") map (_.text)
    val value = (node \ "@value").text
    val extraValue = (node \ "@extraValue").text
    val loopPath = (node \ "@loopPath").text
    op match {
      case "IS NULL" => Success(NullConstraint(path, EqualTo()))
      case "IS NOT NULL" => Success(NullConstraint(path, NotEqualTo()))
      case "ONE OF" => Success(MultiConstraint(path, OneOf(), values))
      case "NONE OF" => Success(MultiConstraint(path, NoneOf(), values))
      case "LOOKUP" => Success(LookupConstraint(path, value, Option(extraValue)))
      case "IN" => Success(ListConstraint(path, In(), value))
      case "NOT IN" => Success(ListConstraint(path, NotIn(), value))
      case "=" => if (loopPath != null && !loopPath.isEmpty()) 
    	  			Success(LoopConstraint(path, Is(), loopPath))
    	  		  else 
    	  		    Success(AttributeConstraint(path, EqualTo(), value))
      case "!=" => if (loopPath != null && !loopPath.isEmpty()) 
    	  			Success(LoopConstraint(path, Isnt(), loopPath))
    	  		  else
    	  		    Success(AttributeConstraint(path, NotEqualTo(), value))
      case ">" => Success(AttributeConstraint(path, GreaterThan(), value))
      case "<" => Success(AttributeConstraint(path, LessThan(), value))
      case ">=" => Success(AttributeConstraint(path, GreaterThanOrEqualTo(), value))
      case "<=" => Success(AttributeConstraint(path, LessThanOrEqualTo(), value))
      case "CONTAINS" => Success(AttributeConstraint(path, Contains(), value))
      case "DOES NOT CONTAIN" => Success(AttributeConstraint(path, DoesntContain(), value))
      case "LIKE" => Success(AttributeConstraint(path, Matches(), value))
      case "NOT LIKE" => Success(AttributeConstraint(path, DoesntMatch(), value))
      case _ => Failure("Illegal operator: " + op)
    }
  }
  
  /**
   * Parse a string such as "A and (B or C) and (D or E or F)" to a valid logic tree.
   */
  def toTree(logic: String): Validation[String, LogicalTree[Char]] = {
    // Variables
    var needsOperator          = false
    var notExpectingAnOperator = true
    var opBuf = new StringBuffer()
    var nodesAtExprStart = Map(0 -> 0)
    var depth = 0
    // State
    val nodes: Stack[NonEmptyTree[Char]] = new Stack()
    val ops: Stack[BooleanOperator.BooleanOperator] = new Stack()
    
    // Helper closures
    def addNode(n: NonEmptyTree[Char]) = {nodes.push(n); needsOperator = nodes.size % 2 == 1}
    def addOp(o: BooleanOperator.BooleanOperator) = {ops.push(o); needsOperator = false}
    def endOfExpr(): Validation[String, Unit] = {
        if (nodes.size == 1 && ops.isEmpty)
          Success()
        else if (ops.isEmpty) 
          Failure("missing operator")
        else if (nodes.size < 2)
          Failure("missing an identifier")
        else {
          val op = ops.pop()
          val r = nodes.pop()
          val l = nodes.pop()
          val node = LogicGroup(op, l, r)
          addNode(node)
          Success()
        }
    }
    var seed: Validation[String, LogicalTree[Char]] = Success(EmptyTree())
    val firstPass = logic.foldLeft(seed)((state, c) => c match {
      case 'a' | 'n' | 'o' => state map (x => {opBuf.append(c); x})
      case 'd' |       'r' => state >>= (x => {
        val currentOp = opBuf.toString + c
        opBuf = new StringBuffer()
        if (notExpectingAnOperator)
          Failure("Unexpected operator: " + currentOp)
        else {
          notExpectingAnOperator = true
		  BooleanOperator.values.find(v => v.toString == currentOp.toUpperCase()) match {
		    case None => Failure(currentOp + " is not a valid operator")
            case Some(op) => {addOp(op); Success(x)}
          }
        }
      })
      case '(' => state map (x => {
        notExpectingAnOperator = true
        depth += 1;
        nodesAtExprStart = nodesAtExprStart.updated(depth, nodes.size)
        x
      })
      case ')' => state >>= (x => {
        notExpectingAnOperator = false
        val targetSize = nodesAtExprStart.getOrElse(depth, 0) + 1
        depth -= 1;
        if (depth < 0)
          Failure("Unmatched right bracket")
        else {
          var res = endOfExpr map (_ => x)
          while (res.isSuccess && nodes.size > targetSize) {
            res = endOfExpr map (_ => x)
          }
          res
        }
      })
      case ' ' => state
      case code if validCodes.contains(code) => state >>= (x => {
        notExpectingAnOperator = false
        if (needsOperator)
          Failure("Unexpected identifier: " + code)
        else {
          addNode(LogicalNode(code));
          Success(x)
        }
      })
      case _ => Failure("Illegal character in logic string: " + c)
    })
    // Mandatory end of expression evaluation
    var res = firstPass >>= (_ => endOfExpr)
    // Use up any available operations
    while (res.isSuccess && ops.size > 0) {
      res = endOfExpr
    }
    // report
    res >>= (_ => if (nodes.size != 1) 
        Failure("Logic does not have a single root") 
      else if (depth != 0) 
        Failure("Unmatched left bracket") 
      else 
        Success(nodes.pop))
  }
  
  def newQuery(s: Service, r: String): Validation[String, Query] = for {
    path <- Path.parse(s.model, r)
    root <- path match {case rp:RootPath => Success(rp); case _ => Failure(r + " does not represent a root path")}
  } yield new Query(s, root, Seq(), Seq(), Map(), EmptyTree(), Map())
  
  def star(m: Model, ps: String, scm:Map[ReferencePath, Table]): Seq[String] = {
    val res = Path.parse(m, ps.replaceAll("\\.?\\*$", ""), scm)
    res.fold(e => Nil, p => p match {
      case x:EndsInTable => x.endTable.attributes.map(a => x.toString() + "." + a.name).sorted
      case _ => Nil
    })
  }
}