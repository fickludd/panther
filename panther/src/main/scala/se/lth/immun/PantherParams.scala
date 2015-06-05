package se.lth.immun

import java.net.InetSocketAddress
import se.jt.Params
import scala.util.{Try, Success, Failure}

class PantherParams extends Params {

	import Params._
	
	val startSpectrumIndex = 0 			## "First spectrum to use"
	val lastSpectrumIndex = 0 			## "Last spectrum to use"
	val mockBig = false					## "set to use big mock data instead of real"
	val address = "localhost:12345" 	## "address to offert MSData service on. IP-ADDRESS:PORT"
	val specQueueSize = 10				## "spectrum to parse in parallell"
	
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