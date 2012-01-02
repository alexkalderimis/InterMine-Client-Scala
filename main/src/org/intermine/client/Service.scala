package org.intermine.client

import org.intermine.client.model.Model
import org.intermine.client.model.ModelParser

import org.apache.http.client._
import org.apache.http.impl.client._
import org.apache.http.client.methods._

class Service(root:String) {

    val httpclient = new DefaultHttpClient
    
	val model:Model = ModelParser.parse(fetch(Service.MODEL))
	
	def fetch(path:String) : String = {
      val method = new HttpGet(root + path)
      val responseHandler = new BasicResponseHandler
      httpclient.execute(method, responseHandler)
	}
}

object Service {
  val MODEL = "/model"
  val SUFFIX = "/service"
}