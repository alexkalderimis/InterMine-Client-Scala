package org.intermine.client.query.constraint

sealed abstract class ConstraintOp() {
  def xmlAttr: String
}

sealed abstract class AttributeOp extends ConstraintOp

// Attribute Operators

sealed abstract class NullOp extends AttributeOp

case class EqualTo extends NullOp {val xmlAttr = "="}
case class NotEqualTo extends NullOp {val xmlAttr = "!="}

case class GreaterThan extends AttributeOp {val xmlAttr = ">"}
case class LessThan extends AttributeOp {val xmlAttr = "<"}
case class GreaterThanOrEqualTo extends AttributeOp {val xmlAttr = ">="}
case class LessThanOrEqualTo extends AttributeOp {val xmlAttr = "<="}
case class Contains extends AttributeOp {val xmlAttr = "CONTAINS"}
case class DoesntContain extends AttributeOp {val xmlAttr = "DOES NOT CONTAIN"}
case class Matches extends AttributeOp {val xmlAttr = "LIKE"}
case class DoesntMatch extends AttributeOp {val xmlAttr = "NOT LIKE"}

// Lookup Operator
case class Lookup extends ConstraintOp {val xmlAttr = "LOOKUP"}

// Loop Operators
sealed abstract class LoopOp extends ConstraintOp 

case class Is extends LoopOp {val xmlAttr = "="}
case class Isnt extends LoopOp {val xmlAttr = "!="}

// Multi-Value operators
sealed abstract class MultiOp extends ConstraintOp

case class OneOf extends MultiOp {val xmlAttr = "ONE OF"}
case class NoneOf extends MultiOp {val xmlAttr = "NONE OF"}

// List Operators
sealed abstract class ListOp extends ConstraintOp

case class In extends ListOp {val xmlAttr = "IN"}
case class NotIn extends ListOp {val xmlAttr = "NOT IN"}