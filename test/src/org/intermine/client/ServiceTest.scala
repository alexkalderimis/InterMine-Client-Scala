package org.intermine.client

import org.junit.Assert
import org.junit.Test

class ServiceTest2() {

  val s : Service = new Service("http://squirrel.flymine.org/intermine-test/service")
  
  @Test
  def fetchModel() = {
    val m = s.model
    Assert.assertEquals("Should have the right name", "testmodel", m.getName)
  }
  
  @Test
  def getTable() = {
    val m = s.model
    m.getTable("Employee").fold(
        e => Assert.fail(e),
        x => Assert.assertEquals("The table has the right name", "Employee", x.name))
    m.getTable("Foo").fold(
        e => (),
        x => Assert.fail("Foo should not have been found, got: " + x))
  }
  
  @Test
  def fields() = {
    for (ceo <- s.model.getTable("CEO")) {
      val attrs = ceo.attributes
      val expectedAttrNames = List("age", "end", "fullTime", "name", "salary", "seniority", "title")
      Assert.assertEquals("They have the right names.", expectedAttrNames, attrs.map((a) => a.name).sorted)
      val refs = ceo.references
      val expectedRefNames = List("address", "company", "department", "departmentThatRejectedMe", "secretarys", "simpleObjects")
      Assert.assertEquals("They have the right names.", expectedRefNames, refs.map((a) => a.name).sorted)
    }
  }
}