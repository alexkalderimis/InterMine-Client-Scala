package org.intermine.client

import org.apache.http.client.methods._
import org.apache.http.client._
import org.apache.http.impl.client._
import org.apache.http.params.HttpParams
import org.intermine.client.model.AttributePath
import org.intermine.client.model.Model
import org.intermine.client.model.ModelParser
import org.intermine.client.query.Queriable
import org.intermine.client.query.Query
import org.intermine.client.query.Template
import org.intermine.client.results.Page
import scalaz._
import org.apache.http.HttpResponse
import org.apache.http.util.EntityUtils
import org.apache.http.message.BasicNameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import java.util.Arrays
import scala.xml.XML
import java.util.Date

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
    
    def getAll(q: Queriable): Validation[String, Seq[Seq[Any]]] = getPage(q)(Page.ALL)
    
    def getPage(q: Queriable)(implicit page: Page): Validation[String, Seq[Seq[Any]]] = {
      val method = new HttpPost(root + q.path)
      val params = resultParams ++ q.asQueryParameters ++ page.toParams
      val nvps = for (pair <- params) yield pair match {case (k, v) => new BasicNameValuePair(k, v)}
      val ent = new UrlEncodedFormEntity(Arrays.asList(nvps.toList:_*), "UTF-8")
      method.setEntity(ent)
      val responseHandler = new Service.TypedResultsHandler(q.views)
      httpclient.execute(method, responseHandler)
    }
    
    def resultParams: Map[String, String] = Map("format" -> "xml")
}

object Service {
  
  import Scalaz._
  
  val MODEL = "/model"
  val TEMPLATES = "/templates"
  val SUFFIX = "/service"
    
  class TypedResultsHandler(view: Seq[AttributePath]) extends ResponseHandler[Validation[String, Seq[Seq[Any]]]] {
    
    val getParser: AttributePath => String => Any = path => path.end.dataType match {
      case "java.lang.String"           => (v => v)
      case "java.lang.Integer" | "int"  => (v => v.toInt)
      case "java.lang.Long"    | "long" => (v => v.toLong)
      case "java.lang.Boolean" | "boolean" => (v => v.toBoolean)
      case "java.lang.Double"  | "double" => (v => v.toDouble)
      case "java.lang.Short"   | "short" => (v => v.toShort)
      case "java.util.Date"             => (v => new Date(v.toLong))
    }
    
    val parsers = view map getParser
    
    def handleResponse(resp: HttpResponse): Validation[String, Seq[Seq[Any]]] = {
      val status = resp.getStatusLine()
      val ent = resp.getEntity()
      val res = if (ent != null) {
        try {
          val xml = XML.load(ent.getContent())
          if (status.getStatusCode() == 200) {
            Success((xml \ "Result") map (r => (r \ "i") map (_.text) zip parsers map (_ match {case (v, p) => p(v)})))
          } else {
            Failure((xml \ "error" \ "cause").text)
          }
        } finally {
          EntityUtils.consume(ent)
        }
      } else {
        Failure(status.getReasonPhrase())
      }
      res
    }
  }
    
  val RESULT_HANDLER = new ResponseHandler[Validation[String, Seq[Seq[String]]]]() {
    
    def handleResponse(resp: HttpResponse): Validation[String, Seq[Seq[String]]] = {
      val line = resp.getStatusLine
      val ent = resp.getEntity()
      val res = if (ent != null) {
        try {
          val xml = XML.load(ent.getContent());
          if (line.getStatusCode() == 200)
            Success((xml \ "Result") map (row => (row \ "i") map (_.text)))
          else
            Failure((xml \ "error" \ "cause").text)
        } finally {
          EntityUtils.consume(ent)
        }
      } else {
        Failure(line.getReasonPhrase())
      }
      res
    }
  }
   
}