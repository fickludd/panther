package se.lth.immun

import java.net.InetSocketAddress
import akka.actor._

object CatSightPrimaries {
	case class PlotID(assayId:Int, source:InetSocketAddress) {
		override def toString = "[%d, %s]".format(assayId, source)
	}
	case class AssayTrace(plotter:ActorRef, plotBuffer:PlotBuffer)
	class Source(
			val name:String, 
			val initiator:ActorRef
	) {
		var connection:Option[ActorRef] = None
	}
}