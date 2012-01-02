package org.intermine.client.query.constraint
import org.intermine.client.query.constraint.BooleanOperator._

class LogicGroup[T](op:BooleanOperator, left:LogicalTree[T], right:LogicalTree[T]) extends LogicalTree[T] {

}
