package org.intermine.client.query

import org.junit.Assert
import org.junit.Test
import org.intermine.client.Service

class QueryTest {
	val s = new Service("http://squirrel.flymine.org/intermine-test/service")
 
	@Test
	def select() = {
	  val expected = <query model="testmodel" 
                    view="Employee.name Employee.age Employee.fullTime"
			  		sortOrder="Employee.name asc"/>
	    
	  Query.newQuery(s, "Employee")
	       .flatMap(_.select("name", "age", "fullTime"))
	       .fold(Assert.fail, s => Assert.assertEquals(expected, s.toXML))
	}
	
	@Test
	def badselect() = {
	  val res = for {
	    q <- Query.newQuery(s, "Employee")
	    q2 <- q.select("name", "age", "foo", "fullTime")
	  } yield q2
	  
	  res.fold(
	      e => Assert.assertEquals("Could not find foo on Employee", e),
	      s => Assert.fail("Expected error - got: " + s))
	      
      Query.newQuery(s, "Department")
         .flatMap(q => q.select("employees.seniority"))
         .fold(println, s => Assert.fail("Expected error - got: " + s))
	}
	
	@Test
	def refselect() = {
	  Query.newQuery(s, "Employee")
	       .flatMap(_.select("name", "age", "department"))
	       .fold(Assert.assertEquals("Employee.department is not an attribute", _), 
	    		   s => Assert.fail("Expected error - got: " + s))
	}
	
	@Test
	def starselect() = {
	  val expected = <query model="testmodel" 
                            view="Employee.age Employee.end Employee.fullTime Employee.name"
                            sortOrder="Employee.age asc"/>
	  Query.newQuery(s, "Employee")
	  	.flatMap(q => q.select("*"))
	  	.fold(Assert.fail, s => Assert.assertEquals(expected,s.toXML))        
	}
	
	@Test
	def refstarselect() = {
	  val expected = <query model="testmodel" 
                            view="Employee.department.company.name Employee.department.company.vatNumber"
                            sortOrder="Employee.department.company.name asc"/> 
	  Query.newQuery(s, "Employee")
	    .flatMap(q => q.select("department.company.*"))
	    .fold(Assert.fail, s => Assert.assertEquals(expected, s.toXML))        
	}
	
	@Test
	def subclassselect() = {
	  var expected = <query view="Department.employees.age Department.employees.end Department.employees.fullTime Department.employees.name Department.employees.seniority Department.employees.title" 
                            model="testmodel"
                            sortOrder="Department.employees.age asc"/> 
	  Query.newQuery(s, "Department")
         .flatMap(q => q.subclassing("employees", "Manager"))
         .flatMap(q => q.select("employees.*"))
         .fold(Assert.fail, s => Assert.assertEquals(expected, s.toXML))
    
      expected = <query view="Department.employees.name Department.employees.seniority" 
      				    model="testmodel"
      					sortOrder="Department.employees.name asc"></query>
      Query.newQuery(s, "Department")
         .flatMap(q => q.subclassing("employees", "Manager"))
         .flatMap(q => q.select("employees.name", "employees.seniority"))
         .fold(Assert.fail, s => Assert.assertEquals(expected, s.toXML))
	}
	
	@Test
	def badsubclass() = {
	  val actions = List(
	      (q:Query) => q.subclassing("employees", "Bank"), 
	      (q:Query) => q.subclassing("foos", "Manager"),
	      (q:Query) => q.subclassing("employees", "Foo"),
	      (q:Query) => q.subclassing("manager", "Employee"),
	      (q:Query) => q.subclassing("name", "Employee")
	      )
	  for (f <- actions) {
		  Query.newQuery(s, "Department")
             .flatMap(f)
             .flatMap(q => q.select("employees.*"))
             .fold(println, s => Assert.fail("Expected error, got: " + s))
	  }
	}
	
	@Test
	def ordering() = {
	  var expected = <query view="Employee.name Employee.age Employee.end" 
                            model="testmodel" 
                            sortOrder="Employee.age asc"/>
	  Query.newQuery(s, "Employee")
	       .flatMap(_.select("name", "age", "end"))
	       .flatMap(_.orderBy("age"))
	       .fold(Assert.fail, s => Assert.assertEquals(expected, s.toXML))
	       
     expected = <query view="Employee.name Employee.age Employee.end Employee.department.name" 
                            model="testmodel" 
                            sortOrder="Employee.department.name asc"/>
	  Query.newQuery(s, "Employee")
	       .flatMap(_.select("name", "age", "end", "department.name"))
	       .flatMap(_.orderBy("department.name"))
	       .fold(Assert.fail, s => Assert.assertEquals(expected, s.toXML))
	       
	}
	
	@Test
	def badordering() = {
	  val badSOEs = List("fop", "department.name")
	  badSOEs.foreach(soe => Query.newQuery(s, "Employee")
		       .flatMap(_.select("name", "age", "end"))
		       .flatMap(_.orderBy(soe))
		       .fold(println, s => Assert.fail("Expected error - got: " + s)))
	}
}