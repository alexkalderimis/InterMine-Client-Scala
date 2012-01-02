package org.intermine.client.query

object Joins extends Enumeration {
	type JoinStyle = Value
	val INNER, OUTER = Value
}