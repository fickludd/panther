package se.lth.immun

import akka.actor._
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._
import scala.swing.Dimension

import se.lth.immun.protocol.MSDataProtocolActors
import se.lth.immun.protocol.MSDataProtocol.Traces
import se.lth.immun.protocol.MSDataProtocol.MasterReply

import se.jt.{ Scale, Stratifier, LinePlot, Util }

object TracePlotter {
	def props(swingActor:ActorRef) =
		Props(classOf[TracePlotter], swingActor)
		
	case class Datum(rt:Double, intensity:Double, id:String)
	case class PopZoom(n:Int)
	case class SetZoomFilter(f:(Datum, Int) => Boolean)
}

class TracePlotter(swingActor:ActorRef) extends Actor {

	import TracePlotter._
	import Scale._
	import Stratifier._
	
	var size = new Dimension(1000, 600)
	var plot:Option[LinePlot[Datum]] = None
	
	def receive = {
		case reply:MasterReply =>
			if (reply.hasStatus)
				println(reply.getStatus.getStatusMsg)
			if (reply.hasTraces) {
				plot = Some(getTracePlot(reply.getTraces))
				swingActor ! Util.drawToBuffer(size.width, size.height, plot.get)
			
			}
			
		case msg:MSDataProtocolActors.MSDataProtocolMsg =>
			println(msg)
			
		case d:Dimension =>
			size = d
			plot.foreach(p =>
					swingActor ! Util.drawToBuffer(size.width, size.height, p)
				)
				
		case PopZoom(n) =>
			plot match {
				case None => {}
				case Some(p) if p.filters.nonEmpty =>
					p.filters.pop
					swingActor ! Util.drawToBuffer(size.width, size.height, p)
						
				case Some(_) => {}
			}
			
			
		case SetZoomFilter(f) =>
			plot match {
				case None => {}
				case Some(p) =>
					p.filters.push(f)
					swingActor ! Util.drawToBuffer(size.width, size.height, p)
						
			}
	}
	
	
	def getTracePlot(t:Traces) = {
		val data = new ArrayBuffer[Datum]
		println("Precursors:")
		for (pTrace <- t.getPrecursorList) {
			val id = "prec %.4f".format(pTrace.getPrecursor.getLmz)
			val trace = pTrace.getTrace.getTimeList.zip(pTrace.getTrace.getIntensityList)
			println(id + " \t" + trace.take(10).mkString)
			data ++= trace.map(t => Datum(t._1, t._2, id))
		}
		println("Fragments:")
		for (fTrace <- t.getFragmentList) {
			val id = "frag %.4f -> %.4f".format(fTrace.getFragment.getPrecursor.getLmz, fTrace.getFragment.getFragment.getLmz)
			val trace = fTrace.getTrace.getTimeList.zip(fTrace.getTrace.getIntensityList)
			println(id + " \t" + trace.take(10).mkString)
			data ++= trace.map(t => Datum(t._1, t._2, id))
		}
		println("trying to plot %d data points".format(data.length))
		new LinePlot(data)
				.x(_.rt)
				//.y(_.intensity)(log10Scale, doubleStratifier)
				.y(_.intensity)
				.color(_.id)
	}
}