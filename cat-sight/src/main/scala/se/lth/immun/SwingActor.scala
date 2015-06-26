package se.lth.immun

import akka.actor._
import scala.swing._
import scala.swing.event._
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.net.InetSocketAddress
import java.awt.image.BufferedImage

import se.lth.immun.protocol.MSDataProtocol
import se.lth.immun.protocol.MSDataProtocolActors
import se.jt.{Util, PlotsControl}

import scala.collection.mutable.HashMap

object SwingActor {
	import PlotBuffer._
	
	def props(params:CatSightParams) =
		Props(classOf[SwingActor], params)
		
	case class InitSourceDefs(sourceDefs:Seq[SourceDef])
	case class InitTraML(path:String)
}

class SwingActor(params:CatSightParams) extends Actor with Reactor {

	import MSDataProtocolActors._
	import CatSightPrimaries._
	import TracePlotter._
	import SwingActor._
	import PlotBuffer._
	
	val gui = new GUI(params)
	listenTo(gui.addAssays)
	listenTo(gui.clearAssays)
	listenTo(gui.posFilterField)
	listenTo(gui.negFilterField)
	listenTo(gui.plots)
	listenTo(gui.saveAssays)
	listenTo(gui.zoomMode.selection)
	listenTo(gui.selectMode.selection)
	listenTo(gui.hideLegends)
	listenTo(gui.addSource)
	listenTo(gui.savePeaks)
	listenTo(gui.assayList.selection)
	
	val sources = new HashMap[InetSocketAddress, Source]
	val assayTraces = new HashMap[PlotID, AssayTrace]
	val assayPeaks = new HashMap[PlotID, AssayTracePeak]
	//val client = context.actorOf(MSDataProtocolActors.ClientInitiator.props(server, self))
	//var msDataConnection:ActorRef = _
	
	var loadAssayParams = new LoadAssayParams
	var assays:Seq[Assay] = Nil
	def setAssays(a:Seq[Assay]) = {
		assays = a
		updateAssayList
	}
	
	def addSource(s:SourceDef) = {
		val address = new InetSocketAddress(s.ip, s.port)
		val connection = context.actorOf(MSDataProtocolActors.ClientInitiator.props(address, self))
		sources += address -> new Source(s.name, connection)
		gui.setSources(sources)
	}
	
	def updateAssayList = {
		println("updating assay list")
		gui.assayList.listData = 
			assays.filter(a => gui.posFilterField.pos(a.id) && gui.negFilterField.neg(a.id))
		gui.filterStatus.text = "%d of %d assays".format(gui.assayList.listData.length, assays.length)
	}
	
	
	import java.awt.event.KeyEvent
	import java.awt.event.InputEvent
	GlobalKeyBinder.bindings += ((KeyEvent.VK_Z, InputEvent.SHIFT_MASK), () => gui.zoomMode.next)
	GlobalKeyBinder.bindings += ((KeyEvent.VK_S, InputEvent.SHIFT_MASK), () => gui.selectMode.next)
	
	reactions += {
		case e:ButtonClicked if e.source == gui.addAssays =>
			val dialog = new AddAssaysDialog( 
				newAssays => setAssays(assays ++ newAssays),
				loadAssayParams
			)
			dialog.open
			
		case e:ButtonClicked if e.source == gui.clearAssays =>
			setAssays(Nil)
			
		case e:ValueChanged if e.source == gui.posFilterField =>
			updateAssayList
			
		case e:ValueChanged if e.source == gui.negFilterField =>
			updateAssayList
			
		case e:ButtonClicked if e.source == gui.saveAssays =>
			val savePathChooser = new FileChooser(new File("."))
			savePathChooser.showSaveDialog(gui.plots)
			if (savePathChooser.selectedFile != null)
				writeAssays(savePathChooser.selectedFile)
			
		case e:ButtonClicked if e.source == gui.savePeaks =>
			val savePathChooser = new FileChooser(new File("."))
			savePathChooser.showSaveDialog(gui.plots)
			if (savePathChooser.selectedFile != null)
				writePeaks(savePathChooser.selectedFile)
		/*
		case e:UIElementResized if e.source == gui.plots =>
			for (AssayTrace(tp, buff) <- assayTraces.values)
				tp ! buff.size
			*/
		case sc:SelectionChanged if sc.source == gui.assayList =>
			requestAssayIndices(gui.assayList.selection.indices.toSet)
			
		case sc:SelectionChanged if sc.source == gui.zoomMode =>
			println("setting zoom mode to "+gui.zoomMode.selection.item)
			zoomBehaviour = gui.zoomMode.selection.item
			
		case sc:SelectionChanged if sc.source == gui.selectMode =>
			selectBehaviour = gui.selectMode.selection.item
			
		case e:ButtonClicked if e.source == gui.hideLegends =>
			for ((id, aTrace) <- assayTraces)
				aTrace.plotter ! (if (gui.hideLegends.selected) HideLegend else ShowLegend)
				
		case e:ButtonClicked if e.source == gui.addSource =>
			val dialog = new AddSourceDialog( newSourceOpt => 
				for (s <- newSourceOpt) addSource(s)
			)
			dialog.open
			
		case e:PlotBufferZoomEvent =>
			if (assayTraces.contains(e.id)) 
				zoomBehaviour.send(e, assayTraces)
			
		case e:Select =>
			if (assayTraces.contains(e.id)) 
				selectBehaviour.send(e, assayTraces)
				
		case e:PlotBufferEvent =>
			for (AssayTrace(plotter, buff) <- assayTraces.get(e.id)) 
				plotter ! e
	}
	
	def receive = {
		case s:String =>
			println(s)
			
		case MSDataProtocolConnected(remote, local) =>
			sources.get(remote) match {
				case Some(source) =>
					source.connection = Some(sender)
				case None =>
					println("SWINGACTOR: got unknown connection msg to "+remote)
			}
		
		case MSDataReply(msg, nBytes, checkSum, timeTaken, remote) =>
			println("CatSight| parsed %d bytes in %5d ms. CHECKSUM=%12d".format(nBytes, timeTaken, checkSum))
			//val id = PlotID(msg.getId, remote)
			sources(remote).onCompleted(msg.getId)
			assayTraces.find(_._1.intID == msg.getId) match {
				case Some((id, at)) =>
					at.plotter ! msg -> assays.find(_.id == id.assayId).get
				case None => println("SWINGACTOR: got msg for non-existant plotID "+msg.getId)
			}
			
		case TracePlotter.PlotUpdate(id, plot, ctrl) =>
			assayTraces.get(id) match {
				case Some(at) =>
					at.plotBuffer.setImg(plot)
					at.plotBuffer.setControl(ctrl)
				case None => println("SWINGACTOR: got plotUpdate for non-existant plotBuffer "+id)
			}
			
		case InitSourceDefs(sourceDefs) =>
			for (sd <- sourceDefs) addSource(sd)
			
		case lap:LoadAssayParams =>
			loadAssayParams = lap
			
		case InitTraML(path) =>
			val dialog = new AddAssaysDialog( 
				newAssays => setAssays(assays ++ newAssays),
				loadAssayParams,
				Some(path)
			)
			dialog.open
			
		case atp:AssayTracePeak =>
			println("SWINGACTOR: got assayTracePeak: "+atp)
			if (assayPeaks.contains(atp.id))
				assayPeaks(atp.id) = atp
			else
				assayPeaks += atp.id -> atp
			
	}
	
	
	var zoomBehaviour:PlotActionBehaviour = PlotActionBehaviour.Self
	var selectBehaviour:PlotActionBehaviour = PlotActionBehaviour.Self
	
	var lastSelectedAssayIds:Set[Int] = Set()
	def requestAssayIndices(assayIds:Set[Int]) = {
		if (assayIds != lastSelectedAssayIds) {
			lastSelectedAssayIds = assayIds
			val reqSet = assayIds.flatMap(aid => 
					sources.keys.map(addr => PlotID(gui.assayList.listData(aid).id, addr))
				)
			val currSet = assayTraces.keys.toSet
			val toRemove = currSet -- reqSet
			val stillIn = currSet & reqSet
			val toAdd = reqSet -- currSet
			for (id <- toRemove) {
				val at = assayTraces(id)
				context.stop(at.plotter)
				deafTo(at.plotBuffer)
			}
			assayTraces --= toRemove
			
			for (at <- stillIn.map(assayTraces)) at.plotBuffer.clear
			for (id <- toAdd) {
				val tp = context.actorOf(TracePlotter.props(self, id, gui.hideLegends.selected))
				val buff = new PlotBuffer(id)
				listenTo(buff)
				assayTraces += id -> AssayTrace(tp, buff)
				sources(id.source).query(assays.find(_.id == id.assayId).get.toTraceMsg(id.intID, params))
			}
			
			gui.setPlots(assayTraces)
			for (at <- assayTraces.values)
				at.plotter ! at.plotBuffer.size
		}
	}
	
	def writeAssays(f:File) = {
		val assays = if (gui.saveAll.enabled) this.assays else gui.assayList.listData
		val w = new BufferedWriter(new FileWriter(f))
		
		def writeRow(qoute:Boolean)(a:Any*) = 
			w.write(a.map(_ match {
				case s:String => 
					if (qoute) params.outQuote + s + params.outQuote
					else s
				case x => x.toString
			}).mkString(params.outSep) + "\n")
			
		writeRow(false)(
				"assayId",
				"traceId",
				"precMz",
				"fragMz"
			)
			
		for (a <- assays) {
			for (p <- a.precs)
				writeRow(true)(a.id, p.id, p.mz, "-")
			for (f <- a.frags)
				writeRow(true)(a.id, f.id, f.precMz, f.fragMz)
		}
		w.close()
	}
	
	def writePeaks(f:File) = {
		val peaks = assayPeaks.values.toSeq.sortBy(_.id.assayId)
		val w = new BufferedWriter(new FileWriter(f))
		
		def writeRow(qoute:Boolean)(a:Any*) = 
			w.write(a.map(_ match {
				case s:String => 
					if (qoute) params.outQuote + s + params.outQuote
					else s
				case x => x.toString
			}).mkString(params.outSep) + "\n")
			
		writeRow(false)(
				"assayID",
				"file",
				"peakID",
				"intensitySum",
				"intensityMax",
				"rtStart",
				"rtApex",
				"rtEnd"
			)
			
		for (p <- peaks)
			writeRow(false)(
					p.id.assayId,
					sources(p.id.source).name,
					p.peakID,
					"%.5e".format(p.intensitySum),
					"%.5e".format(p.intensityMax),
					"%.4f".format(p.rtStart),
					"%.4f".format(p.rtApex),
					"%.4f".format(p.rtEnd)
				)
		
		w.close()
	}
}