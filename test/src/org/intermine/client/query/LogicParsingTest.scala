package org.intermine.client.query
import org.junit.Test
import org.junit.Assert
import org.intermine.client.query.constraint.LogicalTree

class LogicParsingTest {

  @Test
  def simpleExpression() = {
    val logic = "X and Y"
    Query.toTree(logic)
         .fold(Assert.fail, tree => Assert.assertEquals("A and B", LogicalTree.toLogicString(tree)))
    Query.toTree("R or S")
         .fold(Assert.fail, tree => Assert.assertEquals("A or B", LogicalTree.toLogicString(tree)))
    Query.toTree("RorS")
         .fold(Assert.fail, tree => Assert.assertEquals("A or B", LogicalTree.toLogicString(tree)))     
  }
  
  @Test
  def outOfOrder() = {
    Query.toTree("R S or")
         .fold(println, tree => Assert.fail("Expected error - got: " + tree))
  }
  
  @Test
  def compoundExpression() = {
    Query.toTree("(R or S) and (X or Y)")
         .fold(Assert.fail, tree => Assert.assertEquals("(A or B) and (C or D)", LogicalTree.toLogicString(tree)))
  }
  
  @Test
  def flatExpression() = {
    Query.toTree("R and S and X and Y")
         .fold(Assert.fail, tree => Assert.assertEquals("A and B and C and D", LogicalTree.toLogicString(tree)))
    Query.toTree("R and S or X and Y")
         .fold(Assert.fail, tree => Assert.assertEquals("A and (B or (C and D))", LogicalTree.toLogicString(tree)))
  }
  
  @Test
  def unbalanced() = {
    Query.toTree("(R and S) or X")
         .fold(Assert.fail, tree => Assert.assertEquals("(A and B) or C", LogicalTree.toLogicString(tree)))
  }
  
  @Test
  def bracketing() = {
    val exp = "(A or (B and (C or D))) and (E or (F and (G or H)))"
    Query.toTree("(Q or R and S or T) and (V or U and W or X)")
         .fold(Assert.fail, tree => Assert.assertEquals(exp, LogicalTree.toLogicString(tree)))
  }
  
  @Test
  def complex() = {
    val expected = "A and ((B and (C or D) and E) or (F and (G or H) and (I or J)))"
    Query.toTree("Q and (R and (M or N) and S) or (X and (Y or Z) and (F or G))")
         .fold(Assert.fail, tree => Assert.assertEquals(expected, LogicalTree.toLogicString(tree)))
  }
  
  @Test
  def unmatchedBrackets() = {
    Query.toTree("Q and (R and S").fold(println, tree => Assert.fail("Expected error - got: " + tree))
    Query.toTree("Q and R) and S").fold(println, tree => Assert.fail("Expected error - got: " + tree))
    Query.toTree("(Q and) R and S").fold(println, tree => Assert.fail("Expected error - got: " + tree))
    Query.toTree("Q (or R) and S").fold(println, tree => Assert.fail("Expected error - got: " + tree))
  }
  
  @Test
  def bracketed() = {
    Query.toTree("(X and Y and Z)")
         .fold(Assert.fail, tree => Assert.assertEquals("A and B and C", LogicalTree.toLogicString(tree)))
  } 
  
  @Test
  def singleNode() = {
    Query.toTree("X")
         .fold(Assert.fail, tree => Assert.assertEquals("A", LogicalTree.toLogicString(tree)))
  } 
  
  @Test
  def unrooted() = {
    Query.toTree("Q S").fold(println, tree => Assert.fail("Expected error - got: " + tree))
    Query.toTree("(Q and S) R").fold(println, tree => Assert.fail("Expected error - got: " + tree))
  }
  
  @Test
  def operitis() = {
    Query.toTree("Q and and S").fold(println, tree => Assert.fail("Expected error - got: " + tree))
  }
    
}