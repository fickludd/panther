package se.lth.immun

import akka.actor._
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._
import scala.swing.Dimension

import se.lth.immun.protocol.MSDataProtocolActors
import se.lth.immun.protocol.MSDataProtocol.Traces
import se.lth.immun.protocol.MSDataProtocol.MasterReply

import se.jt.{ Scale, Stratifier, LinePlot, Util, PlotsControl }
import java.awt.image.BufferedImage

import CatSightPrimaries._

object TracePlotter {
	def props(swingActor:ActorRef, id:PlotID, hideLegend:Boolean) =
		Props(classOf[TracePlotter], swingActor, id, hideLegend)
		
	case class Datum(rt:Double, intensity:Double, id:String)
	trait TracePlotterMsg
	//case class PopZoom(n:Int) extends TracePlotterMsg
	//case class SetZoomFilter(f:(Datum, Int) => Boolean) extends TracePlotterMsg
	case object HideLegend extends TracePlotterMsg
	case object ShowLegend extends TracePlotterMsg
	case class RemoveAssayTracePeak(peakID:Int) extends TracePlotterMsg
	
	case class PlotUpdate(id:PlotID, plot:BufferedImage, control:PlotsControl[Datum, Datum, Datum])
}

class TracePlotter(swingActor:ActorRef, id:PlotID, hideLegend:Boolean) extends Actor {

	import TracePlotter._
	import Scale._
	import Stratifier._
	import PlotBuffer._
	import PeakIntegrator._
	
	var size = new Dimension(1000, 600)
	var plot:Option[LinePlot[Datum]] = None
	val assayPeakPlot = new AssayPeakPlot
	val peakIDs = new IDGenerator
	
	def receive = {
		case (reply:MasterReply, assay:Assay) =>
			if (reply.hasStatus)
				println(reply.getStatus.getStatusMsg)
			if (reply.hasTraces) {
				plot = Some(getTracePlot(reply.getTraces, assay))
				swingActor ! plotUpdate(plot.get)
			}
			
		case msg:MSDataProtocolActors.MSDataProtocolMsg =>
			println(msg)
			
		case NewSize(_, d) =>
			if (d.getHeight > 0 && d.getWidth > 0)
			size = d
			plot.foreach(p => swingActor ! plotUpdate(p))
				
		case Select(fromID, f) =>
			println("NEW SELECT from %s to %s ".format(fromID, id))
			for (p <- plot) {
				val integrator = context.actorOf(Props[PeakIntegrator], "integrator")
				def filter(data:Seq[Datum], f:(Datum, Int) => Boolean) =
					for {
						i <- 0 until data.length
						if f(data(i),i)
					} yield data(i)
						
				integrator ! PeakIntegrator.Integrate(id, peakIDs.next, filter(p.data, f), swingActor)
			}
			
		case atp:AssayTracePeak =>
			assayPeakPlot.peaks.clear
			assayPeakPlot.peaks += atp
			for (p <- plot)
				swingActor ! plotUpdate(p)
				
		case RemoveAssayTracePeak(peakID) =>
			assayPeakPlot.remove(peakID)
			for (p <- plot)
				swingActor ! plotUpdate(p)
			
		case PopZoom(_, n) =>
			for (p <- plot)
				if (p.filters.nonEmpty) {
					p.filters.pop
					swingActor ! plotUpdate(p)
				}
			
		case NewZoom(_, f) =>
			for (p <- plot) {
				p.filters.push(f)
				swingActor ! plotUpdate(p)
			}
			
		case HideLegend =>
			for (p <- plot) 
				if (!p.hideLegends) {
					p.hideLegends = true
					swingActor ! plotUpdate(p)
				}
			
		case ShowLegend =>
			for (p <- plot) 
				if (p.hideLegends) {
					p.hideLegends = false
					swingActor ! plotUpdate(p)
				}
	}
	
	
	def plotUpdate(p:LinePlot[Datum]) = {
		val imgCtrl = Util.drawToBuffer(size.width, size.height, p)
		PlotUpdate(id, imgCtrl.img, imgCtrl.control)
	}
	
	
	
	def getTracePlot(t:Traces, assay:Assay) = {
		val data = new ArrayBuffer[Datum]
		for (prec <- assay.precs) {
			t.getPrecursorList.find(t => 
				prec.mz > t.getPrecursor.getLmz && 
				prec.mz < t.getPrecursor.getHmz
			) match {
				case None => println("TracePlotter: cannot match trace with assay!")
				case Some(pTrace) =>
					data ++= 
						(if (pTrace.hasSmallTrace) {
							val time = pTrace.getSmallTrace.getTimeList
							val int = pTrace.getSmallTrace.getIntensityList
							for (i <- 0 until time.length) 
								yield Datum(time(i).toDouble, int(i).toDouble, prec.id)
							
						} else {
							val time = pTrace.getTrace.getTimeList
							val int = pTrace.getTrace.getIntensityList
							for (i <- 0 until time.length) 
								yield Datum(time(i), int(i), prec.id)
						})
					
			}
		}
		
		for (frag <- assay.frags) {
			t.getFragmentList.find(t =>
				frag.precMz > t.getFragment.getPrecursor.getLmz &&
				frag.precMz < t.getFragment.getPrecursor.getHmz &&
				frag.fragMz > t.getFragment.getFragment.getLmz &&
				frag.fragMz < t.getFragment.getFragment.getHmz
			) match {
				case None => println("TracePlotter: cannot match trace with assay!")
				case Some(fTrace) =>
					data ++= 
						(if (fTrace.hasSmallTrace) {
							val time = fTrace.getSmallTrace.getTimeList
							val int = fTrace.getSmallTrace.getIntensityList
							for (i <- 0 until time.length) 
								yield Datum(time(i).toDouble, int(i).toDouble, frag.id)
							
						} else {
							val time = fTrace.getTrace.getTimeList
							val int = fTrace.getTrace.getIntensityList
							for (i <- 0 until time.length) 
								yield Datum(time(i), int(i), frag.id)
						})
			}
		}
		val p = new LinePlot(data)
				.x(_.rt)
				.y(_.intensity)
				.color(_.id)
		p.hideLegends = hideLegend
		p.overlays += assayPeakPlot
		p
	}
}