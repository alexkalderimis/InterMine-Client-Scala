package org.intermine.client.model
import scalaz._

trait Path {
  
  def append(part:String) : Validation[String, Path]
  
  def parent : Validation[String, Path]
}

trait EndsInTable {
  
  def endTable: Table
  def model: Model
  def root: Table
  def refs: Seq[Reference]
  def subclasses: Map[ReferencePath, Table]
  
  def append(part:String) : Validation[String, Path] = {
    val t = endTable
    t.getField(part).map(f => f match {
      case a:Attribute => new AttributePath(model, root, refs, a, subclasses)
      case r:Reference => new ReferencePath(model, root, refs ++ Seq(r), subclasses)
      })
  }
}

object Path {
  
  import Scalaz._
  
  def parse(model:Model, pathString:String) : Validation[String, Path] = parse(model, pathString, Map())
  
  def parse(model:Model, pathString:String, subclasses:Map[ReferencePath, Table]) : Validation[String, Path] = {
    val parts = List.fromArray(pathString.split("\\."))
    return parts match {
      case Nil => failure[String, Path]("No parts to process in " + pathString)
      case root :: rest => {
        val seed = model.getTable(root).map(t => new RootPath(model, t, subclasses):Path)
        rest.foldLeft(seed)((path, part) => path.fold(
              e => Failure("Could not append " + part + ": " + e),
              s => s.append(part))) 
      } 
    }
  }
}