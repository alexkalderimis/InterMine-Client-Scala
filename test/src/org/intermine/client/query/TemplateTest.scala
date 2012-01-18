package org.intermine.client.query

import scalaz.Validation.Monad._

import org.junit.Assert
import org.junit.Test
import org.intermine.client.Service
import org.intermine.client.query.constraint.Constraint._
import org.intermine.client.query.Query._

import scala.xml.Utility

class TemplateTest {

  val s = new Service("http://squirrel.flymine.org/intermine-test/service")

  @Test
  def names() {
    val templates = s.templates
    Assert.assertTrue("All templates parse successfully", !templates.exists(_.isFailure))
    templates map (vt => vt map (t => println(t.name + ", " + t.title)))
  }

}