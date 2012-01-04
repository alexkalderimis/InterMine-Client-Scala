package org.intermine.client.query.constraint
import org.junit.Assert
import org.junit.Test

class ConstraintTreeTest {
  
  @Test
  def toLogicString() = {
    val tree = LogicGroup(BooleanOperator.AND, LogicalNode("X"), LogicalNode("Y"))
    
    Assert.assertEquals("(A and B)", LogicalTree.toLogicString(tree))
    
    val tree2 = LogicGroup(BooleanOperator.OR, LogicalNode("X"), LogicalNode("Y"))
    
    Assert.assertEquals("(A or B)", LogicalTree.toLogicString(tree2))
  }
  
  @Test
  def nestedLogic() = {
    val tree = LogicGroup(BooleanOperator.OR,
    			LogicGroup(BooleanOperator.AND, LogicalNode(1), LogicalNode(2)),
    			LogicGroup(BooleanOperator.AND,
    			    LogicalNode(3),
    			    LogicGroup(BooleanOperator.OR, LogicalNode(4), LogicalNode(5))))

    Assert.assertEquals("((A and B) or (C and (D or E)))", LogicalTree.toLogicString(tree))
    
    val tree2 = LogicGroup(BooleanOperator.OR,
    			LogicGroup(BooleanOperator.AND, LogicalNode(1), LogicalNode(2)),
    			LogicGroup(BooleanOperator.AND,
    			    LogicGroup(BooleanOperator.AND,
    			        LogicGroup(BooleanOperator.OR, LogicalNode(4), LogicalNode(5)),
    			        LogicalNode(6)),	
    			    LogicGroup(BooleanOperator.OR, LogicalNode(7), LogicalNode(8))))
    Assert.assertEquals("((A and B) or (((C or D) and E) and (F or G)))", LogicalTree.toLogicString(tree2))    			    
  }
  
  @Test
  def emptyLogic() = {
    val tree = EmptyTree()
    
    Assert.assertEquals("", LogicalTree.toLogicString(tree))
  }

}