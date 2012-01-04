package org.intermine.client.query.constraint

import scalaz._
import Scalaz._
import org.intermine.client.query.constraint.BooleanOperator._

sealed abstract class LogicalTree[A]

case class EmptyTree[A]() extends LogicalTree[A]

sealed abstract class NonEmptyTree[A] extends LogicalTree[A] {
 def or(that: NonEmptyTree[A]) = LogicGroup(BooleanOperator.OR, this, that)
 def and(that: NonEmptyTree[A]) = LogicGroup(BooleanOperator.AND, this, that)
}

case class LogicGroup[A](op: BooleanOperator, l: NonEmptyTree[A], r: NonEmptyTree[A]) extends NonEmptyTree[A]
case class LogicalNode[A](v: A) extends NonEmptyTree[A]

object LogicalTree {

  implicit def NonEmptyTreeFunctor: Functor[NonEmptyTree] = new Functor[NonEmptyTree] {
	def fmap[A, B](t: NonEmptyTree[A], f: A => B): NonEmptyTree[B] = t match {
	  case LogicalNode(v) => LogicalNode(f(v))
	  case LogicGroup(op, l, r) => LogicGroup(op, fmap(l, f), fmap(r, f))
	}
  }
  
  implicit def EmptyTreeFunctor: Functor[EmptyTree] = new Functor[EmptyTree] {
    def fmap[A, B](t: EmptyTree[A], f: A => B): EmptyTree[B] = EmptyTree()
  }
  
  implicit def LogicalTreeFuntor: Functor[LogicalTree] = new Functor[LogicalTree] {
    def fmap[A, B](t: LogicalTree[A], f: A => B): LogicalTree[B] = t match {
      case e @ EmptyTree() => EmptyTreeFunctor.fmap(e, f)
      case ne:NonEmptyTree[A] => NonEmptyTreeFunctor.fmap(ne, f)
    } 
  }
  
  implicit def FoldableLogicalTree: Foldable[LogicalTree] = new Foldable[LogicalTree] {
    override def foldRight[A, B](t: LogicalTree[A], z: => B, f: ((A, => B) => B)): B = t match {
      case EmptyTree() => z	
      case LogicalNode(v) => f(v, z)
      case LogicGroup(_, l, r) => foldRight(r, foldRight(l, z, f), f)
    }
  }
  
  def toLogicString[A](tree: LogicalTree[A]): String = {
    val codes = 'A'.to('Z').iterator
    val f = (_:Any) => codes.next() 
    def bs(t: LogicalTree[Char], z: String, f: (Char, String) => String): String = t match {
      case EmptyTree() => z
      case LogicalNode(c) => f(c, z)
      case LogicGroup(op, l, r) => {
        val processSubTree: NonEmptyTree[Char] => String = st => st match {
          case LogicalNode(c) => f(c, z)
          case lg @ LogicGroup(so, _, _) if so == op => bs(lg, "", f)
          case lg @ LogicGroup(so, _, _) if so != op => "(" + bs(lg, "", f) + ")"
        }
        processSubTree(l) + " " + op.toString.toLowerCase + " " + processSubTree(r) 
      }
    }
    bs(tree map f, "", (a, b) => b + a)
  }
   
}
