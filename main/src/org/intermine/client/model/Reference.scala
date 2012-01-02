package org.intermine.client.model

class Reference(fieldname:String, refersTo:String, isReferedToBy:String, isColl:Boolean) extends Field {
	def name = fieldname
	def dataType = refersTo
	def referencedBy = isReferedToBy
	def isCollection = isColl
}