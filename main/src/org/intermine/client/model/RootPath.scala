package org.intermine.client.model
import scalaz.Validation
import scalaz.Success
import scalaz.Failure

protected class RootPath(val model:Model, val root:Table, val subclasses:Map[ReferencePath, Table]) extends Path with EndsInTable {

  override def toString = root.name
  
  def endTable = root
  def refs = Seq()
  
  def parent = Failure("Root paths cannot have parents")
  
  override def equals(obj:Any) : Boolean = obj.isInstanceOf[Path] && (toString == obj.toString())
}