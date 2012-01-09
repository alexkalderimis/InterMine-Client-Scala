package org.intermine.client

import org.intermine.client.model.Model
import org.intermine.client.model.ModelParser
import scalaz._
import org.apache.http.client._
import org.apache.http.impl.client._
import org.apache.http.client.methods._
import org.intermine.client.query.Query
import org.intermine.client.query.Template

class Service(root:String) {

    val httpclient = new DefaultHttpClient
    
	val model:Model = ModelParser.parse(fetch(Service.MODEL))
	
	def templates = Template.parseTemplates(this, fetch(Service.TEMPLATES))
	
	def fetch(path:String) : String = {
      val method = new HttpGet(root + path)
      val responseHandler = new BasicResponseHandler
      httpclient.execute(method, responseHandler)
	}
    
    def from(className: String) = Query.newQuery(this, className)
}

object Service {
  
  import Scalaz._
  
  val MODEL = "/model"
  val TEMPLATES = "/templates"
  val SUFFIX = "/service"
   
}