package se.lth.immun

import se.jt.Params

class PantherParams extends Params {

	import Params._
	
	val startSpectrumIndex = 0 			## "First spectrum to use"
	val lastSpectrumIndex = 0 			## "Last spectrum to use"
	val mockBig = false					## "set to use big mock data instead of real"
	val address = "localhost:12345" 	## "address to offert MSData service on. IP-ADDRESS:PORT"
	
	val mzML = ReqString("needed file ")
}