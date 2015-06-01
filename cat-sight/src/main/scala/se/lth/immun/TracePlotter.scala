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
}

class TracePlotter(swingActor:ActorRef) extends Actor {

	import TracePlotter._
	import Scale._
	import Stratifier._
	
	var size = new Dimension(1000, 600)
	var data:Option[ArrayBuffer[Datum]] = None
	
	def receive = {
		case t:Traces =>
			println("got traces!!")
			
		case reply:MasterReply =>
			println("got master reply!!")
			if (reply.hasStatus)
				println(reply.getStatus.getStatusMsg)
			if (reply.hasTraces) {
				data = Some(formatTraces(reply.getTraces))
				swingActor ! plotTraces(data.get)
			
			}
			
		case msg:MSDataProtocolActors.MSDataProtocolMsg =>
			println(msg)
			
		case d:Dimension =>
			size = d
			data.foreach(data =>
					swingActor ! plotTraces(data)
				)
	}
	
	
	def formatTraces(t:Traces) = {
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
		data
	}
	
	
	def plotTraces(data:ArrayBuffer[Datum]) = {
		val plot = new LinePlot(data)
					.x(_.rt)
					//.y(_.intensity)(log10Scale, doubleStratifier)
					.y(_.intensity)
					.color(_.id)
		
		
		Util.drawToBuffer(size.width, size.height, plot)
	}
}