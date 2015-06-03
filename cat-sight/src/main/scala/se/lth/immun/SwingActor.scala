package se.lth.immun

import akka.actor._
import scala.swing.Reactor
import scala.swing.event._
import java.net.InetSocketAddress
import java.awt.image.BufferedImage

import se.lth.immun.protocol.MSDataProtocol
import se.lth.immun.protocol.MSDataProtocolActors
import se.jt.{Util, PlotsControl}

object SwingActor {
	def props(params:CatSightParams) =
		Props(classOf[SwingActor], params)
}

class SwingActor(params:CatSightParams) extends Actor with Reactor {

	import MSDataProtocolActors._
	
	val gui = new GUI(params)
	listenTo(gui.addAssays)
	listenTo(gui.plotBuffer)
	listenTo(gui.assayList.selection)
	
	val server = new InetSocketAddress("localhost", 12345)
	val tracePlotter = context.actorOf(TracePlotter.props(self))
	val client = context.actorOf(MSDataProtocolActors.ClientInitiator.props(server, self))
	var msDataConnection:ActorRef = _
	
	
	reactions += {
		case e:ButtonClicked if e.source == gui.addAssays =>
			val dialog = new AddAssaysDialog( assays => 
				gui.assayList.listData = gui.assayList.listData ++ assays
			)
			dialog.open
		
		case e:UIElementResized if e.source == gui.plotBuffer =>
			tracePlotter ! e.source.size
			
		case sc:SelectionChanged if sc.source == gui.assayList =>
			gui.assayList.selection.indices.headOption match {
				case Some(index) =>
					requestAssayIndex(index)
				case None => {}
			}
	}
	
	gui.plotBuffer.onNewZoom = f => tracePlotter ! TracePlotter.SetZoomFilter(f)
	gui.plotBuffer.onZoomPop = n => tracePlotter ! TracePlotter.PopZoom(n)
	
	def receive = {
		case s:String =>
			println(s)
			
		case MSDataProtocolConnected(remote, local) =>
			msDataConnection = sender
			gui.assayList.selectIndices(0)
			tracePlotter ! gui.plotBuffer.size
 
		case MSDataReply(msg, nBytes, checkSum, timeTaken) =>
			println("CatSight| parsed %d bytes in %d ms. CHECKSUM=%d".format(nBytes, timeTaken, checkSum))
			tracePlotter ! msg
			
		case Util.ImgControl(plot, ctrl) =>
			gui.plotBuffer.setImg(plot)
			gui.plotBuffer.setControl(ctrl)
			
	}
	
	var currIndex = -1
	def requestAssayIndex(i:Int) = {
		if (i != currIndex && i >= 0 && i < gui.assayList.listData.length) {
			currIndex = i
			msDataConnection ! gui.assayList.listData(i).toTraceMsg(params)
		}
	} 
}