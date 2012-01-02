package org.intermine.client.model
import scalaz.Failure
import scalaz.Success

protected class AttributePath(model:Model, root:Table, refs:Seq[Reference], end:Attribute, subclasses:Map[ReferencePath, Table]) extends Path { 
  
  override def toString = root.name + refs.foldLeft("")((ac, r) => ac + "." + r.name) + "." + end.name
  
  def append(part:String) = Failure("This path cannot be appended to, as it ends in an attribute")
  
  def parent = if (refs.isEmpty) 
    Success(new RootPath(model, root, subclasses))
  else 
    Success(new ReferencePath(model, root, refs, subclasses))

  override def equals(obj:Any) : Boolean = obj.isInstanceOf[AttributePath] && (toString == obj.toString())
}