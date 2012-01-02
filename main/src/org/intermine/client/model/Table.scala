package org.intermine.client.model
import scalaz.Validation
import scalaz.Success
import scalaz.Failure

class Table(unqualifiedName:String, parents:Seq[String], attrDescs : Seq[Attribute], refDescs : Seq[Reference]) {
  def name = unqualifiedName
  def attributes = attrDescs
  def references = refDescs
  def directSuperClasses = parents
  
  def getField(fieldName:String) : Validation[String, Field] = if ("id" == fieldName) Success(Table.ID_ATTRIBUTE) else 
    attrDescs find ((a) => a.name == fieldName) match {
      case Some(a) => Success(a)
      case None => (refDescs find ((r) => r.name == fieldName)) match {
        case Some(r) => Success(r)
        case None => Failure("Could not find " + fieldName + " on " + name)
      }
  }
  
  override def toString = "Table(" + unqualifiedName + ")"
  
  def inheritsFrom(putativeParent: String): Boolean = parents.contains(putativeParent)
  def inheritsFrom(putativeParent: Table): Boolean = inheritsFrom(putativeParent.name)
}

object Table {
  val ID_ATTRIBUTE = new Attribute("id", "java.lang.Integer")
}