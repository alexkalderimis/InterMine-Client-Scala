package org.intermine.client.query

import org.intermine.client.Service
import scalaz._
import scala.xml.XML
import scala.xml.Elem

class Template(
    val name: String, 
    val title: String, 
    val description: String, 
    val comment: String, 
    val query: Query)

object Template {
  def parseTemplate(s: Service, src: String): Validation[String, Template] = {
    val xml = XML.loadString(src)
    parseTemplate(s, xml)
  }
  
  def parseTemplate(s: Service, src: scala.xml.Node): Validation[String, Template] = {
    val name = (src \ "@name").text
    val title = (src \ "@title").text
    val desc = (src \ "@longdescription").text
    val comment = (src \ "@comment").text
    val qs = (src \ "query")
    if (qs.size != 1) {
      Failure("Wrong number of query elements: " + qs.size)
    } else {
      //for (q <- Query.fromXML(s, qs.first)) yield new Template(name, title, q)
      Query.fromXML(s, qs.first).fold(e => Failure("Error parsing " + name + "(" + src + "): " + e),
                                      q => Success(new Template(name, title, desc, comment, q)))
    }
  }
  
  def parseTemplates(s: Service, src: String): Seq[Validation[String, Template]] = parseTemplates(s, XML.loadString(src))
  def parseTemplates(s: Service, src: Elem) = (src \ "template") map (e => parseTemplate(s, e))
}