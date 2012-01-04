package org.intermine.client.query

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
import org.intermine.client.query.constraint._
import org.intermine.client.Service

import org.intermine.client.query.constraint.NonEmptyTree
import org.intermine.client.query.constraint.LogicalTree._

import scalaz._


class Query(
    val service: Service, 
    val root: RootPath, 
    val views: Seq[AttributePath], 
    val sortOrder: Seq[Pair[AttributePath, SortDirection]], 
    val subclasses: Map[ReferencePath, Table], 
    val constraints: LogicalTree[Constraint[Path]],
    val join: Map[ReferencePath, JoinStyle]) { 
  
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
  
  /** 
   * Add a series of sort-order and direction pairs to the query.
   */
  def orderBy(soes: (String, SortDirection)*): Validation[String, Query] = {
    soes.foldLeft(validation[String, Query](Right(this)))((v, soe) => soe match { case (p, d) => v.flatMap(q => q.orderBy(p, d))})
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
    val f: (Constraint[String] => Validation[String, Constraint[Path]]) = (a) => a match {
      case AttributeConstraint(p, o, v) => for (path <- asPath(p)) yield AttributeConstraint(path, o, v)
      case LookupConstraint(p, v, e) => for (path <- asPath(p)) yield LookupConstraint(path, v, e)
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
  def newQuery(s: Service, r: String): Validation[String, Query] = for {
    root <- Path.parse(s.model, r)
  } yield new Query(s, root.asInstanceOf[RootPath], Seq(), Seq(), Map(), EmptyTree(), Map())
  
  def star(m: Model, ps: String, scm:Map[ReferencePath, Table]): Seq[String] = {
    val res = Path.parse(m, ps.replaceAll("\\.?\\*$", ""), scm)
    res.fold(e => Nil, p => p match {
      case x:EndsInTable => x.endTable.attributes.map(a => x.toString() + "." + a.name).sorted
      case _ => Nil
    })
  }
}