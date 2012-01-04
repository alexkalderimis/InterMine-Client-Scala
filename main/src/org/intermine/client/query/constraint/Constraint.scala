package org.intermine.client.query.constraint
import scala.xml.Elem

sealed abstract class Constraint[A] {
  def path: A
  def op: ConstraintOp
  def toElem: Elem
  def unary_!(): Constraint[A] 
}

case class NullConstraint[A](val path: A, val op: NullOp) extends Constraint[A] {
  def toElem = op match {
    case EqualTo() => <constraint path={path.toString} op="IS NULL"/>
    case NotEqualTo() => <constraint path={path.toString} op="IS NOT NULL"/>
  }
  def unary_! = op match {
    case EqualTo() => NullConstraint(path, NotEqualTo())
    case NotEqualTo() => NullConstraint(path, EqualTo())
  }
}

case class AttributeConstraint[A](val path: A, val op: AttributeOp, val value: Any) extends Constraint[A] {
  def toElem = <constraint path={path.toString} op={op.xmlAttr} value={value.toString}/>
  def unary_! = op match {
    case EqualTo() => AttributeConstraint(path, NotEqualTo(), value)
    case NotEqualTo() => AttributeConstraint(path, EqualTo(), value)
    case GreaterThan() => AttributeConstraint(path, LessThanOrEqualTo(), value)
    case GreaterThanOrEqualTo() => AttributeConstraint(path, LessThan(), value)
    case LessThan() => AttributeConstraint(path, GreaterThanOrEqualTo(), value)
    case LessThanOrEqualTo() => AttributeConstraint(path, GreaterThan(), value)
    case Contains() => AttributeConstraint(path, DoesntContain(), value)
    case DoesntContain() => AttributeConstraint(path, Contains(), value)
    case Matches() => AttributeConstraint(path, DoesntMatch(), value)
    case DoesntMatch() => AttributeConstraint(path, Matches(), value)
  }
}

case class LookupConstraint[A](val path: A, val value: Any, val extra: Option[String]) extends Constraint[A] {
  val op = Lookup()
  def toElem = extra match {
    case None => <constraint path={path.toString} op="LOOKUP" value={value.toString}/> 
    case Some(x) => <constraint path={path.toString} op="LOOKUP" value={value.toString} extraValue={x.toString}/> 
  }
  def unary_! = this
}

case class LoopConstraint[A](val path: A, val op: LoopOp, val loopPath: A) extends Constraint[A] {
  def toElem = <constraint path={path.toString} op={op.xmlAttr} loopPath={loopPath.toString}/>
  def unary_! = op match {
    case Is() => LoopConstraint(path, Isnt(), loopPath)
    case Isnt() => LoopConstraint(path, Is(), loopPath)
  }
}
case class MultiConstraint[A](val path: A, val op: MultiOp, val values: Seq[Any]) extends Constraint[A] {
  def toElem = <constraint path={path.toString} op={op.xmlAttr}>{values map (x => <value>{x.toString}</value>)}</constraint>
  def unary_! = op match {
    case OneOf() => MultiConstraint(path, NoneOf(), values)
    case NoneOf() => MultiConstraint(path, OneOf(), values)
  }
}
case class ListConstraint[A](val path: A, val op: ListOp, val listName: String) extends Constraint[A] {
  def toElem = <constraint path={path.toString} op={op.xmlAttr} value={listName}/>
  def unary_! = op match {
    case In() => ListConstraint(path, NotIn(), listName)
    case NotIn() => ListConstraint(path, In(), listName) 
  } 
}

class ConstrainableString(str:String) {
  def |>|(v: Any) = new AttributeConstraint(str, GreaterThan(), v)
  def |>=|(v: Any) = new AttributeConstraint(str, GreaterThanOrEqualTo(), v)
  def |<|(v: Any) = new AttributeConstraint(str, LessThan(), v)
  def |<=|(v: Any) = new AttributeConstraint(str, LessThanOrEqualTo(), v)
  def |==|(v: Any) = v match {
    case null => new NullConstraint(str, EqualTo())
    case _    => new AttributeConstraint(str, EqualTo(), v)
  }
  def |!=|(v: Any) = v match {
    case null => new NullConstraint(str, NotEqualTo())
    case _    => new AttributeConstraint(str, NotEqualTo(), v)
  }
  def CONTAINS(v: Any) = new AttributeConstraint(str, Contains(), v)
  def IN(v:String) = new ListConstraint(str, In(), v)
  def |~~|(v: Any) = new LookupConstraint(str, v, None)
  def |~~|(v: (Any, String)) = v match {case (a, b) => new LookupConstraint(str, a, Some(b))}
  def ONEOF(vs: Seq[Any]) = MultiConstraint(str, OneOf(), vs)
  def NONEOF(vs: Seq[Any]) = MultiConstraint(str, NoneOf(), vs)
}

object Constraint {
  
  implicit def str2constrainable(str:String) = new ConstrainableString(str)
  
  implicit def con2tree[A](con: Constraint[A]): NonEmptyTree[Constraint[A]] = new LogicalNode(con)
  
}