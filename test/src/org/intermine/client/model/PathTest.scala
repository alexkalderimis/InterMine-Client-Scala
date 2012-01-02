package org.intermine.client.model
import org.intermine.client.Service
import org.junit.Assert
import org.junit.Test

import scalaz._

class PathTest {
  
  val s = new Service("http://squirrel.flymine.org/intermine-test/service")
  
  def testGoodPath[C](pathString:String) = {
    val res = Path.parse(s.model, pathString)
    res.fold(e => Assert.fail(e), p => Assert.assertEquals("Stringifies correctly", pathString, p.toString))
  }
  
  def testGoodPath[C](pathString:String, subclasses:Map[ReferencePath, Table]) = {
    val res = Path.parse(s.model, pathString, subclasses)
    res.fold(e => Assert.fail(e), p => Assert.assertEquals("Stringifies correctly", pathString, p.toString))
  }
  
  def testBadPath(pathString:String) = {
    val res = Path.parse(s.model, pathString)
    res.fold(e => (), p => Assert.fail("Expected error - got: " + p))
  }
  
  @Test
  def parseRootPath() = {
    testGoodPath[RootPath]("Employee")
    testGoodPath[RootPath]("CEO")
    testGoodPath[RootPath]("Bank")
  }
  
  @Test
  def parseBadRootPath() = {
    testBadPath("Foo")
  }
  
  @Test
  def parseIdPaths() = {
    testGoodPath("Employee.id")
    testGoodPath("Employee.department.id")
    testGoodPath("Employee.department.company.id")
  }
  
  @Test
  def withOneRef() = {
    testGoodPath[ReferencePath]("Employee.department")
    testGoodPath[ReferencePath]("CEO.company")
    testGoodPath[ReferencePath]("Bank.debtors")
  }
  
  @Test
  def withOneBadRef() = {
    testBadPath("Employee.foo")
  }
  
  @Test
  def withMultipleRefs() = {
    testGoodPath[ReferencePath]("Employee.department.company")
    testGoodPath[ReferencePath]("CEO.company.departments.employees.address")
  }
  
  @Test
  def withBadMultiRefs() = {
    testBadPath("Employee.department.address")
    testBadPath("CEO.company.departments.foo.address")
    testBadPath("CEO.company.departments.name.address")
  }
  
  @Test
  def withSingleAttr() = {
    testGoodPath[AttributePath]("Employee.name")
    testGoodPath[AttributePath]("Employee.age")
    testGoodPath[AttributePath]("CEO.seniority")
    testGoodPath[AttributePath]("Bank.name")
  }
  
  @Test
  def withBadAttrs() = {
    testBadPath("Employee.notName")
  }
  
  @Test
  def withAllParts() = {
    testGoodPath[AttributePath]("Employee.department.name")
    testGoodPath[AttributePath]("CEO.company.departments.manager.address.address")
  }
  
  @Test
  def subclasses() = {
    val m = s.model
    val scs = for {
      dep <- m.getTable("Department")
      ceo <- m.getTable("CEO")
      man <- dep.getField("manager")
    } yield Map(new ReferencePath(m, dep, Seq(man.asInstanceOf[Reference]), Map()) -> ceo)
    if (scs.isFailure) Assert.fail("Some part of setup failed")
    for (scm <- scs) {
      testGoodPath("Department.manager.salary", scm)
        testGoodPath("Department.manager.company.name", scm)
        testBadPath("Department.manager.company.name")
        val result1 = for {
            agePath <- Path.parse(m, "Department.manager.age", scm)
            parent <- agePath.parent
            salaryPath <- parent.append("salary") 
        } yield Assert.assertEquals("Department.manager.salary", salaryPath.toString())
        Assert.assertTrue(result1.isSuccess)
        val result2 = for {
            agePath <- Path.parse(m, "Department.manager.age", scm)
            parent <- agePath.parent
            f <- parent.append("debtors") 
        } yield Assert.fail("Should never get here")
        Assert.assertTrue(result2.isFailure) 
    }
  }

}