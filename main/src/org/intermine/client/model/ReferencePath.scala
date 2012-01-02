package org.intermine.client.model
import scalaz.Validation
import scalaz.Success
import scalaz.Failure

protected class ReferencePath(val model:Model, val root:Table, val refs:Seq[Reference], 
    val subclasses:Map[ReferencePath, Table]) 
	extends Path with EndsInTable {
  
    override def toString = root.name + refs.foldLeft("")((ac, r) => ac + "." + r.name)
    
	def endTable = if (subclasses.contains(this)) subclasses(this) else model.getTable(refs.last.dataType).fold(
	  _ => throw new IllegalStateException("Inconsistent model - reference " + refs.last + " has type " + refs.last.dataType + ", but it is not in the model"),
	  t => t)
	  
	def parent = refs match {
	  case Nil => (new RootPath(model, root, subclasses)).parent
	  case _ :: Nil => Success(new RootPath(model, root, subclasses))
	  case _ => Success(new ReferencePath(model, root, refs.slice(0, refs.size), subclasses))
	}
	
	override def equals(obj:Any) : Boolean = obj.isInstanceOf[Path] && (toString == obj.toString())
}