package org.intermine.client.query
import org.intermine.client.model.AttributePath

trait Queriable {
  
  def asQueryParameters: Map[String, String]
  
  def path: String
  
  def views: Seq[AttributePath]

}