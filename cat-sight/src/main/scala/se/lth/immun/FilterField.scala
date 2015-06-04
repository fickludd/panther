package se.lth.immun

import scala.swing.TextField

class FilterField extends TextField {

	def pos(str:String) = 
		if (text != null && text.nonEmpty)
			str.contains(text)
		else true
		
	def neg(str:String) = 
		if (text != null && text.nonEmpty)
			!str.contains(text)
		else true
}