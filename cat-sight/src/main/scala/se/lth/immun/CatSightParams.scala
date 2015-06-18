package se.lth.immun

import se.jt.Params

class CatSightParams extends Params {

	import Params._
	
	val tracePPM = 20.0			## "extraction window of either side of target trace mass"
	val server = "localhost:12345" 	## "MSData server to connect to. IP-ADDRESS:PORT"
	
	val traML = ReqString("Assays to load")

	val outSep = "\t"
	val outQuote = "\""
}