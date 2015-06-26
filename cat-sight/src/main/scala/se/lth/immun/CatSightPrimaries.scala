package se.lth.immun

import java.net.InetSocketAddress
import akka.actor._

import scala.collection.mutable.Queue
import se.lth.immun.protocol.MSDataProtocol.MasterRequest

object CatSightPrimaries {
	
	var plotIDcounter = 0
	def getID = {
		plotIDcounter += 1
		plotIDcounter
	}
	
	case class PlotID(assayId:String, source:InetSocketAddress) {
		val intID = getID
		override def toString = "[%s, %s]".format(assayId, source)
	}
	case class AssayTrace(plotter:ActorRef, plotBuffer:PlotBuffer)
	class Source(
			val name:String, 
			val initiator:ActorRef
	) {
		var connection:Option[ActorRef] = None
		var waitingFor:Option[Int] = None
		val queue = new Queue[MasterRequest]
		
		def query(mr:MasterRequest) = {
			queue += mr
			if (waitingFor.isEmpty && connection.isDefined) {
				val next = queue.dequeue
				connection.get ! next
				waitingFor = Some(next.getId)
			}
		}
		
		def onCompleted(completedID:Int) = {
			waitingFor match {
				case Some(id) =>
					if (id == completedID) {
						waitingFor = 
							if (queue.nonEmpty) {
								val next = queue.dequeue
								connection.get ! next
								Some(next.getId)
							} else 
								None
					} else 
						println("Source: got complete notification for id '%d' but was waiting for '%d'".format(completedID, id))
				case None =>
					println("Source: got complete notification for id '%d' but was waiting for any job".format(completedID))
			}
		}
	}
	
	
	case class AssayTracePeak(
			id:PlotID, 
			peakID:Int,
			intensitySum:Double, 
			intensityMax:Double, 
			rtStart:Double, 
			rtApex:Double, 
			rtEnd:Double
		)
}