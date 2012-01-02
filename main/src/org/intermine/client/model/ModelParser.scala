package org.intermine.client.model

import scala.xml._

object ModelParser {

  def parse(input:String) : Model = {
    val elem = XML.loadString(input)
    val tables = (elem \\ "model" \\ "class").map((c) => {
        val name = (c \ "@name").text
        val parents = (c \ "@extends").text.split(" ")
        val attrs = (c \\ "attribute").map((a) => new Attribute((a \ "@name").text, (a \ "@type").text))
        val refs = (c \\ "reference").map((r) => new Reference((r \ "@name").text, (r \ "@referenced-type").text, (r \ "@reverse-reference").text, false))
        val cols = (c \\ "collection").map((col) => new Reference((col \ "@name").text, (col \ "@referenced-type").text, (col \ "@reverse-reference").text, true))
    	new Table(name, parents, attrs, refs.union(cols))
      })
    val name = (elem \\ "model" \ "@name").text
    return new Model(name, tables.map((t) => performInheritance(t, tables)))
  }
  
  def performInheritance(table:Table, tables:Seq[Table]) : Table = {
    val allSuperClasses = getAllSuperClasses(Seq(table), tables)
    val superAttrs = allSuperClasses.flatMap((t) => t.attributes)
    val superRefs = allSuperClasses.flatMap((t) => t.references)
    new Table(table.name, allSuperClasses.map((sc) => sc.name), superAttrs, superRefs)
  }
  
  def getAllSuperClasses(supers:Seq[Table], all:Seq[Table]) : Seq[Table] = supers match {
    case Seq() => Seq()
    case items => items ++ getAllSuperClasses(
        items.flatMap((i) => all.filter((t) => i.directSuperClasses.contains(t.name))), all)
  }
}