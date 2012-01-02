package org.intermine.client.model
import scalaz.Validation
import scalaz.Failure
import scalaz.Success

class Model(name:String, tables:Seq[Table]) {
	def getName : String = name
	
	def getTable(name:String) : Validation[String, Table] = tables.find((t) => {t.name == name}) match {
	  case None => Failure("No table named " + name + " in this model")
	  case Some(t) => Success(t)
	}

}