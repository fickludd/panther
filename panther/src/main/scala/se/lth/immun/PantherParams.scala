package se.lth.immun

import java.net.InetSocketAddress
import se.jt.Params
import scala.util.{Try, Success, Failure}

class PantherParams extends Params {

	import Params._
	
	val startSpectrumIndex = 0 			## "First spectrum to use"
	val lastSpectrumIndex = 0 			## "Last spectrum to use"
	val mockBig = false					## "set to use big mock data instead of real"
	val dataStore = "simple"			## "data store to use (simple or flexible). Simple is fast but only works for static DIA windows setups, ei. one SWATH per peptide."
	
	val address = "localhost:12345" 	## "address to offert MSData service on. IP-ADDRESS:PORT"
	val specQueueSize = 10				## "spectrum to parse in parallell"
	val verbose = false					## "set to enable more output"
	val verboseFreq = 100				## "print verbose output every n:th line"
	val force = false					## "attempt to force mzML read even if some required attributes are missing"
	
	val mzML = ReqString("needed file ")
	
	def parseAddress:InetSocketAddress = {
		val parts = address.value.split(":")
		Try(parts.last.toInt) match {
			case Success(port) =>
				val ip = if (parts.length == 2) parts.head else "localhost"
				new InetSocketAddress(ip, port)
				
			case Failure(e) =>
				parts.length match {
					case 1 => new InetSocketAddress(parts.head, 12345)
					case _ => throw new Exception("Unparsable server address '%s'".format(address.value))
				}
		}
	}
}