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

    def getAll(q: Queriable): Validation[String, Seq[Seq[Option[String]]]] = getPage(q)(Page.ALL)

    def getAllAssoc(q: Queriable): Validation[String, Seq[Map[String, String]]] = getAssocPage(q)(Page.ALL)

    def getAssocPage(q: Queriable)(implicit page: Page): Validation[String, Seq[Map[String, String]]] = doRequest(q, page, new Service.AssociativeResultsHandler(q.views))

    def getPage(q: Queriable)(implicit page: Page): Validation[String, Seq[Seq[Option[String]]]] = doRequest(q, page, Service.RESULT_HANDLER)

    def doRequest[T](q: Queriable, page: Page, handler: ResponseHandler[T]): T = {
      val method = new HttpPost(root + q.path)
      val params = resultParams ++ q.asQueryParameters ++ page.toParams
      val nvps = for (pair <- params) yield pair match {case (k, v) => new BasicNameValuePair(k, v)}
      val ent = new UrlEncodedFormEntity(Arrays.asList(nvps.toList:_*), "UTF-8")
      method.setEntity(ent)
      httpclient.execute(method, handler)
    }

    def resultParams: Map[String, String] = Map("format" -> "xml")
}

object Service {

  import Scalaz._

  val MODEL = "/model"
  val TEMPLATES = "/templates"
  val SUFFIX = "/service"

  class TypedResultsHandler(view: Seq[AttributePath]) extends ResponseHandler[Validation[String, Seq[Seq[Option[Any]]]]] {

    val getParser: AttributePath => String => Any = path => path.end.dataType match {
      case "java.lang.String"           => (v => v)
      case "java.lang.Integer" | "int"  => (v => v.toInt)
      case "java.lang.Long"    | "long" => (v => v.toLong)
      case "java.lang.Boolean" | "boolean" => (v => v.toBoolean)
      case "java.lang.Double"  | "double" => (v => v.toDouble)
      case "java.lang.Short"   | "short" => (v => v.toShort)
      case "java.util.Date"             => (v => new Date(v.toLong))
    }

    val parsers = view map getParser map (f => ((v:String) => v match {case "null" => None; case _ => Some(f(v))}))

    def handleResponse(resp: HttpResponse): Validation[String, Seq[Seq[Option[Any]]]] = {
      val status = resp.getStatusLine()
      val ent = resp.getEntity()
      val res = if (ent != null) {
        try {
          val xml = XML.load(ent.getContent())
          if (status.getStatusCode() == 200) {
            Success((xml \ "Result") map (r => (r \ "i") map (_.text) zip parsers map ({case (v, p) => p(v)})))
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

  class ResultRow(pairs: Seq[(AttributePath, String)]) extends Map[String, String] {

    val root = pairs.first._1.root
    val subMap = Map((pairs flatMap {case (p, v) => Seq(p.toString -> v, p.toString.dropWhile(c => c != '.').drop(1) -> v)}):_*)

    def +[B >: String](kv: (String, B)) = Map(((pairs map {case (p, v) => (p.toString() -> v)}) ++ Seq(kv)):_*)

    def -(key: String) = Map((pairs map {case (p, v) => (p.toString() -> v)} filter {case (p, v) => p != key}):_*)

    def get(key: String) = subMap.get(key)

    def iterator: Iterator[(String, String)] = (pairs map {case (p, v) => (p.toString -> v)}).iterator
  }

  class AssociativeResultsHandler(view: Seq[AttributePath]) extends ResponseHandler[Validation[String, Seq[Map[String, String]]]] {

    def handleResponse(resp: HttpResponse): Validation[String, Seq[Map[String, String]]] = {
      val status = resp.getStatusLine()
      val ent = resp.getEntity()
      val res = if (ent != null) {
        try {
          val xml = XML.load(ent.getContent())
          if (status.getStatusCode() == 200) {
            Success((xml \ "Result") map (r => new ResultRow(((r \ "i") map (_.text)).zipWithIndex filter (_._1 != "") map ({case (v, i) => view(i) -> v}))))
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

  val RESULT_HANDLER = new ResponseHandler[Validation[String, Seq[Seq[Option[String]]]]]() {

    def handleResponse(resp: HttpResponse): Validation[String, Seq[Seq[Option[String]]]] = {
      val line = resp.getStatusLine
      val ent = resp.getEntity()
      val res = if (ent != null) {
        try {
          val xml = XML.load(ent.getContent());
          if (line.getStatusCode() == 200)
            Success((xml \ "Result") map (row => (row \ "i") map (_.text) map (v => v match {case "" => None; case _ => Some(v)})))
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
