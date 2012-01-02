package org.intermine.client.model

class Attribute(fieldname:String, contains:String) extends Field {
	def name = fieldname
	def dataType = contains
}