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
	def props(params:CatSightParams) =
		Props(classOf[SwingActor], params)
}

class SwingActor(params:CatSightParams) extends Actor with Reactor {

	import MSDataProtocolActors._
	import CatSightPrimaries._
	import TracePlotter._
	
	val gui = new GUI(params)
	listenTo(gui.addAssays)
	listenTo(gui.clearAssays)
	listenTo(gui.posFilterField)
	listenTo(gui.negFilterField)
	listenTo(gui.plots)
	listenTo(gui.saveAssays)
	listenTo(gui.syncZoom)
	listenTo(gui.hideLegends)
	listenTo(gui.addSource)
	listenTo(gui.assayList.selection)
	
	val sources = new HashMap[InetSocketAddress, Source]
	val assayTraces = new HashMap[PlotID, AssayTrace]
	//val client = context.actorOf(MSDataProtocolActors.ClientInitiator.props(server, self))
	//var msDataConnection:ActorRef = _
	
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
		gui.filterStatus.text = "showing %d of %d assays".format(gui.assayList.listData.length, assays.length)
	}
	
	reactions += {
		case e:ButtonClicked if e.source == gui.addAssays =>
			val dialog = new AddAssaysDialog( newAssays => 
				setAssays(assays ++ newAssays)
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
			savePathChooser.showDialog(gui.plots, null)
			if (savePathChooser.selectedFile != null)
				writeAssays(savePathChooser.selectedFile)
		
		case e:UIElementResized if e.source == gui.plots =>
			for (AssayTrace(tp, buff) <- assayTraces.values)
				tp ! buff.size
			
		case sc:SelectionChanged if sc.source == gui.assayList =>
			requestAssayIndices(gui.assayList.selection.indices.toSet)
			
		case e:ButtonClicked if e.source == gui.syncZoom =>
			zoomBehaviour = 
				if (gui.syncZoom.selected) zoomAll
				else zoomSelf
			
		case e:ButtonClicked if e.source == gui.hideLegends =>
			for ((id, aTrace) <- assayTraces)
				aTrace.plotter ! (if (gui.hideLegends.selected) HideLegend else ShowLegend)
				
		case e:ButtonClicked if e.source == gui.addSource =>
			val dialog = new AddSourceDialog( newSourceOpt => 
				for (s <- newSourceOpt) addSource(s)
			)
			dialog.open
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
			//msDataConnection = sender
		
		case MSDataReply(msg, nBytes, checkSum, timeTaken, remote) =>
			println("CatSight| parsed %d bytes in %5d ms. CHECKSUM=%12d".format(nBytes, timeTaken, checkSum))
			val id = PlotID(msg.getId, remote)
			assayTraces.get(id) match {
				case Some(at) =>
					at.plotter ! msg
				case None => println("SWINGACTOR: got msg for non-existant plotter "+id)
			}
			
		case TracePlotter.PlotUpdate(id, plot, ctrl) =>
			assayTraces.get(id) match {
				case Some(at) =>
					at.plotBuffer.setImg(plot)
					at.plotBuffer.setControl(ctrl)
				case None => println("SWINGACTOR: got plotUpdate for non-existant plotBuffer "+id)
			}
			
	}
	
	
	def zoomSelf(tp:ActorRef, msg:TracePlotterMsg) = tp ! msg
	def zoomAll(tp:ActorRef, msg:TracePlotterMsg) = for (at <- assayTraces.values) at.plotter ! msg
	var zoomBehaviour:(ActorRef, TracePlotterMsg) => Unit = zoomSelf
		
	def updateZoomBehaviour = 
		for (AssayTrace(tp, buff) <- assayTraces.values) {
			buff.onNewZoom = f => zoomBehaviour(tp, SetZoomFilter(f))
			buff.onZoomPop = n => zoomBehaviour(tp, PopZoom(n))	
		}
	
	
	def requestAssayIndices(assayIds:Set[Int]) = {
		val reqSet = assayIds.flatMap(aid => sources.keys.map(addr => PlotID(aid, addr)))
		val currSet = assayTraces.keys.toSet
		val toRemove = currSet -- reqSet
		val stillIn = currSet & reqSet
		val toAdd = reqSet -- currSet
		for (id <- toRemove)
			context.stop(assayTraces(id).plotter)
		assayTraces --= toRemove
		
		for (at <- stillIn.map(assayTraces)) at.plotBuffer.clear
		for (id <- toAdd) {
			val tp = context.actorOf(TracePlotter.props(self, id, gui.hideLegends.selected))
			val buff = new PlotBuffer(id)
			sources(id.source).connection match {
				case Some(conn) =>
					conn ! gui.assayList.listData(id.assayId).toTraceMsg(id.assayId, params)
				case None =>
					println("SWINGACTOR: connection not established for "+id.source)
			}
			assayTraces += id -> AssayTrace(tp, buff)
		}
		
		updateZoomBehaviour
		gui.setPlots(assayTraces)
		for (at <- assayTraces.values)
			at.plotter ! at.plotBuffer.size
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
}