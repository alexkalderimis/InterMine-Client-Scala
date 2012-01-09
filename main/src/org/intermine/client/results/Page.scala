package org.intermine.client.results

class Page(val start: Int, val size: Option[Int]) {
  def toParams: Map[String, String] = size match {
    case None => Map("start" -> start.toString)
    case Some(s) => Map("start" -> start.toString, "size" -> s.toString)
  }
}

object Page {
  
  implicit def defaultPage: Page = new Page(0, Some(10))
  
  val ALL = new Page(0, None)
}