package se.lth.immun

import akka.actor._
import CatSightPrimaries._
import TracePlotter._

object PeakIntegrator {
	case class Integrate(id:PlotID, peakID:Int, data:Seq[Datum], customer:ActorRef)
	
	case class Apex(rt:Double, intensity:Double, traceID:String)
	
	
}

class PeakIntegrator extends Actor {

	import PeakIntegrator._
	
	def receive = {
		case Integrate(id, peakID, data, customer) =>
			val rts 		= data.map(_.rt).distinct.sorted
			val traceIDs 	= data.map(_.id).distinct
			val apexes = 
				for (traceID <- traceIDs) yield findApex(data.filter(_.id == traceID))
			
			val topHalf 	= apexes.sortBy(_.intensity).drop(traceIDs.length / 2)
			val rtApex 		= average(topHalf.map(_.rt))
			val okTraces 	= apexes.filter(a => math.abs(a.rt - rtApex) < 0.1).map(_.traceID)
			val okData 		= data.filter(d => okTraces.contains(d.id))
			val atp = AssayTracePeak(
					id, 
					peakID,
					okData.map(_.intensity).sum, 
					okData.map(_.intensity).max, 
					okData.map(_.rt).min, 
					rtApex, 
					okData.map(_.rt).max
				)
			sender ! atp
			customer ! atp
			
			context.stop(self)
	}
	
	
	def findApex(data:Seq[Datum]) = {
		val apex = data.maxBy(_.intensity)
		Apex(apex.rt, apex.intensity, data.head.id)
	}
	
	
	def average(xs:Seq[Double]) = xs.sum / xs.length
}