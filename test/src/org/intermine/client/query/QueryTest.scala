package org.intermine.client.query

import org.junit.Assert
import org.junit.Test
import org.intermine.client.Service
import org.intermine.client.query.constraint.Constraint._
import org.intermine.client.query.Query._
import scalaz._
import Validation.Monad._
import scala.xml.Utility

class QueryTest 	{
  
	import Scalaz._
	
	val s = new Service("http://squirrel.flymine.org/intermine-test/service")
 
	@Test
	def select() = {
	  val expected = <query model="testmodel" 
                    view="Employee.name Employee.age Employee.fullTime"
			  		sortOrder="Employee.name asc"></query>
	    
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
                            sortOrder="Employee.age asc"></query>
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
	
	@Test
	def joining() = {
	  var expected = scala.xml.Utility.trim(
	                <query view="Employee.name Employee.age Employee.end Employee.address.address" 
                            model="testmodel" 
                            sortOrder="Employee.age asc">
			  			<join path="Employee.address" style="OUTER"/>
			  		</query>)
	  Query.newQuery(s, "Employee")
	       .flatMap(_.select("name", "age", "end", "address.*"))
	       .flatMap(_.outerjoin("address"))
	       .fold(Assert.fail, s => Assert.assertEquals(expected, s.toXML))
	}
	
	@Test
	def singleConstraint() = {
	  var expected = scala.xml.Utility.trim(
	                <query view="Employee.name Employee.age Employee.address.address" 
                            model="testmodel" 
                            sortOrder="Employee.name asc"
			  				constraintLogic="A">
			  			<constraint path="Employee.age" op="&gt;" value="10"></constraint>
			  		</query>)
	  (Query.newQuery(s, "Employee")               >>=
	   (q => q.select("name", "age", "address.*")) >>=
	   (q => q.where("age" |>| 10)))
	   .fold(Assert.fail, s => Assert.assertEquals(expected, s.toXML))
	}	
	
	@Test
	def multiConstraint() = {
     var expected = scala.xml.Utility.trim(
	                <query view="Employee.name Employee.age Employee.address.address" 
                            model="testmodel" 
                            sortOrder="Employee.name asc"
			  				constraintLogic="A and B and C and D and E and F and G and H and I and J and K and L and M and N">
			  			<constraint path="Employee.age" op="&gt;" value="1"/>
                        <constraint path="Employee.age" op="&lt;" value="2"/>
                        <constraint path="Employee.age" op="&lt;=" value="3"/>
                        <constraint path="Employee.age" op="&gt;=" value="4"/>
                        <constraint path="Employee.fullTime" op="=" value="true"/>
                        <constraint path="Employee.name" op="!=" value="fop"/>
                        <constraint path="Employee.name" op="CONTAINS" value="fip"/>
                        <constraint path="Employee.name" op="DOES NOT CONTAIN" value="fup"/>
    		 			<constraint path="Employee.address" op="LOOKUP" value="*Street"/>
                        <constraint path="Employee.address" op="LOOKUP" value="*Street" extraValue="Some Town"/>
                        <constraint path="Employee.address" op="IN" value="some list"/>
                        <constraint path="Employee.address" op="NOT IN" value="some other list"/>
                        <constraint path="Employee" op="IN" value="yet another list"/>
                        <constraint path="Employee.name" op="ONE OF">
                          <value>quux</value>
                          <value>qoox</value>
                          <value>qaax</value>
                        </constraint>
			  		</query>)
	  
	 ((s from "Employee")                     >>= 
	   (_.select("name", "age", "address.*")) >>=    
	   (_.where("age"      |>|  1))           >>=
	   (_.where("age"      |<|  2))           >>=
	   (_.where("age"      |<=| 3))           >>=
	   (_.where("age"      |>=| 4))           >>=
	   (_.where("fullTime" |==| true))        >>=
	   (_.where("name"     |!=| "fop"))       >>=
	   (_.where(  "name"  CONTAINS "fip"))    >>=
	   (_.where(!("name"  CONTAINS "fup")))   >>=  
	   (_.where("address"  |~~| "*Street"))   >>=
	   (_.where("address"  |~~| ("*Street", "Some Town"))) >>=
	   (_.where(  "address" IN "some list"))  >>=
	   (_.where(!("address" IN "some other list") )) >>=
	   (_.where(  "Employee" IN "yet another list")) >>=
	   (_.where("name" ONEOF List("quux", "qoox", "qaax")))
	   )
	    .fold(Assert.fail, s => Assert.assertEquals(expected, s.toXML))
	}
	
	@Test
	def withOrs() = {
	  
	  val expected = <query model="testmodel" view="Employee.name Employee.age"
                            sortOrder="Employee.name asc" constraintLogic="(A or B) and (C or D)" >
                      <constraint path="Employee.name" op="=" value="foo"/>
			  		  <constraint path="Employee.age" op="&gt;=" value="10"/>
                      <constraint path="Employee.fullTime" op="!=" value="true"/>
			  		  <constraint path="Employee.end" op="IS NULL"/>
                    </query>
	  ( (s from "Employee")       >>=
	    (_.select("name", "age")) >>=
	    (_.where((("name"     |==| "foo") or ("age" |>=| 10  )) 
	        and  (("fullTime" |!=| true ) or ("end" |==| null)) )))
	    .fold(Assert.fail, q => Assert.assertEquals(Utility.trim(expected), q.toXML))
	}
	
	@Test
	def allParts() = {
	  val expected = <query model="testmodel" view="Department.name Department.employees.seniority Department.employees.age Department.employees.address.address Department.company.name"
                            sortOrder="Department.name asc Department.employees.age desc Department.employees.fullTime asc" 
                            constraintLogic="(A or B or C) and D" >
			  		  <join path="Department.employees.address" style="OUTER"/>
                      <join path="Department.company" style="OUTER"/>
                      <constraint path="Department.employees" type="Manager"/>
                      <constraint path="Department.name" op="=" value="foo"/>
			  		  <constraint path="Department.employees.age" op="&gt;=" value="10"/>
                      <constraint path="Department.employees.seniority" op="&lt;" value="20"/>
			  		  <constraint path="Department.employees.end" op="IS NOT NULL"/>
                    </query>
	    
	  ( (s from "Department") >>=
	    (_.subclassing("employees", "Manager")) >>=  
	    (_.select("name", "employees.seniority", "employees.age", "employees.address.*", "company.name")) >>=
	    (_.where( (("name" |==| "foo") or ("employees.age" |>=| 10) or ("employees.seniority" |<| 20)) and ("employees.end" |!=| null))) >>=
	    (_.orderBy(("name", Sorting.ASC), ("employees.age", Sorting.DESC), ("employees.fullTime", Sorting.ASC))) >>=
	    (_.outerjoin("employees.address")) >>=
	    (_.outerjoin("company"))
	  ).fold(Assert.fail, q => Assert.assertEquals(Utility.trim(expected), q.toXML))
	  
	}
	
	@Test
	def fromXMLNoConstraints() = {
	  val src = <query model="testmodel" view="Employee.name Employee.age Employee.address.address Employee.department.name" 
                       sortOrder="Employee.department.name asc Employee.age desc" constraintLogic="">
			  		<join path="Employee.address" style="OUTER"/>
			  		<constraint path="Employee.department.manager" type="CEO"/>
			  	</query>
	   
	  Query.fromXML(s, src).fold(Assert.fail, println)
	}
	
	@Test
	def fromXMLWithConstraints() = {
	  val src = <query model="testmodel" view="Employee.name Employee.age Employee.address.address Employee.department.name" 
                       sortOrder="Employee.department.name asc Employee.age desc" constraintLogic="A and B">
			  		<join path="Employee.address" style="OUTER"/>
			  		<constraint path="Employee.department.manager" type="CEO"/>
			  		<constraint path="Employee.name" op="=" value="foo"/>
			  		<constraint path="Employee" op="IN" value="Some List"/>
			  	</query>
	   
	  Query.fromXML(s, src).fold(Assert.fail, println)
	}
	
	@Test
	def fromXMLWithConstraints2() = {
	  val src = <query model="testmodel" view="Employee.name Employee.age Employee.address.address Employee.department.name" 
                       sortOrder="Employee.department.name asc Employee.age desc" constraintLogic="A or B and C">
			  		<join path="Employee.address" style="OUTER"/>
			  		<constraint path="Employee.department.manager" type="CEO"/>
			  		<constraint path="Employee.name" op="=" value="foo" code="C"/>
			  		<constraint path="Employee" op="IN" value="Some List" code="A"/>
                    <constraint path="Employee" op="NOT IN" value="Some List" code="B"/>
			  	</query>
	   
	  Query.fromXML(s, src).fold(Assert.fail, println)
	}
	
	@Test
	def fromXMLBadConstraints() = {
	  val src = <query model="testmodel" view="Employee.name Employee.age Employee.address.address Employee.department.name" 
                       sortOrder="Employee.department.name asc Employee.age desc" constraintLogic="A or B and C">
			  		<join path="Employee.address" style="OUTER"/>
			  		<constraint path="Employee.department.manager" type="CEO"/>
			  		<constraint path="Employee.name" op="=" value="foo" code="C"/>
			  		<constraint path="Employee" op="Wibble" value="Some List" code="A"/>
                    <constraint path="Employee" op="Flibble" value="Some List" code="B"/>
			  	</query>
	   
	  Query.fromXML(s, src).fold(println, q => Assert.fail("Expected error - got: " + q))
	}
	
	@Test
	def disagreeingLogic() = {
	  val src = <query model="testmodel" view="Employee.name Employee.age Employee.address.address Employee.department.name" 
                       sortOrder="Employee.department.name asc Employee.age desc" constraintLogic="A or B">
			  		<join path="Employee.address" style="OUTER"/>
			  		<constraint path="Employee.department.manager" type="CEO"/>
			  		<constraint path="Employee.name" op="=" value="foo" code="C"/>
			  		<constraint path="Employee" op="IN" value="Some List" code="A"/>
                    <constraint path="Employee" op="NOT IN" value="Some List" code="B"/>
			  	</query>
	   
	  Query.fromXML(s, src).fold(println, q => Assert.fail("Expected failure - got: " + q))
	}
	
	@Test
	def fromBadXMLNoConstraints() = {
	  val src = <query model="testmodel" view="Employee.name Employee.age Employee.address.address Employee.department.name" 
                       sortOrder="Employee.department.name asc Employee.age desc" constraintLogic="">
			  		<join path="Employee.address" style="OUTER"/>
			  		<constraint path="Employee.department.manager" type="Badger"/>
			  	</query>
	   
	  Query.fromXML(s, src).fold(println, q => Assert.fail("Expected failure - got: " + q))
	}
	
	@Test
	def results(): Unit = {
	  ( (s from "Employee")       >>=
	    (_.select("name", "age", "department.name")) >>=
	    (_.where( ("name"     |>=| "M") and ("name" |<=| "P") )) >>=
	    (_.byPage))
	    .fold(Assert.fail, rows => rows map (row => println(row(0) + " is " + row(1) + " and works in " + row(2))))
	}
	
	@Test
	def typedresults(): Unit = {
	  ( (s from "Employee")                                      >>=
	    (_.select("name", "age", "department.name"))             >>=
	    (_.where( ("name"     |>=| "M") and ("name" |<=| "P") )) >>=
	    (_.byPage))
	    .fold(Assert.fail, rows => {
	      rows groupBy (_(2)) map {case (d, rs) => println(d + ": " + (rs map (_(1).asInstanceOf[Int])).sum)}
	      println("Total: " +  (rows map (_(1).asInstanceOf[Int])).sum)
	    })
	}
	
	@Test
	def badresults(): Unit = {
	  ( (s from "Employee")       >>=
	    (_.select("name", "age", "department.name")) >>=
	    (_.where( ("age"     |>=| "M") and ("name" |<=| "P") )) >>=
	    (_.byPage))
	    .fold(println, rows => Assert.fail("Expected and error - got: " + rows))
	}
}
