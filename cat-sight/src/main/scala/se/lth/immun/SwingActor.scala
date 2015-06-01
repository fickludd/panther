package se.lth.immun

import akka.actor._
import scala.swing.Reactor
import scala.swing.event._
import java.net.InetSocketAddress
import java.awt.image.BufferedImage

import se.lth.immun.protocol.MSDataProtocol
import se.lth.immun.protocol.MSDataProtocolActors

object SwingActor {
	def props(params:CatSightParams) =
		Props(classOf[SwingActor], params)
}

class SwingActor(params:CatSightParams) extends Actor with Reactor {

	val gui = new GUI(params)
	listenTo(gui.loadAssay)
	listenTo(gui.plotBuffer)
	
	val server = new InetSocketAddress("localhost", 12345)
	val tracePlotter = context.actorOf(TracePlotter.props(self))
	val client = context.actorOf(MSDataProtocolActors.ClientInitiator.props(server, self))
	var msDataConnection:ActorRef = _
	
	reactions += {
		case ButtonClicked(c) =>
			println("button clicked")
			msDataConnection ! requestRandomAssay(3, 6)
			
		case e:UIElementResized if e.source == gui.plotBuffer =>
			tracePlotter ! e.source.size
			
		case e:Event =>
			println(e)
	}
	
	import MSDataProtocolActors._
	
	def receive = {
		case s:String =>
			println(s)
			
		case MSDataProtocolConnected(remote, local) =>
			msDataConnection = sender
			msDataConnection ! requestRandomAssay(3, 6)
			tracePlotter ! gui.plotBuffer.size
 
		case MSDataReply(msg, nBytes, checkSum, timeTaken) =>
			println("CatSight| parsed %d bytes in %d ms. CHECKSUM=%d".format(nBytes, timeTaken, checkSum))
			tracePlotter ! msg
			
		case plot:BufferedImage =>
			gui.plotBuffer.setImg(plot)
			
	}
	
	
	def requestRandomAssay(nPrec:Int, nFrag:Int) = {
		import MSDataProtocol._
		
		val req = GetTracesFor.newBuilder
		for (i <- 0 until nPrec) {
			val (mz, diff) = randMzAndDiff
			req.addPrecursor(Bounds.newBuilder.setLmz(mz-diff).setHmz(mz+diff))
		}
		for (i <- 0 until nFrag) {
			val (mz1, diff1) = randMzAndDiff
			val (mz2, diff2) = randMzAndDiff
			req.addFragment(
				FragmentBounds.newBuilder
					.setPrecursor(Bounds.newBuilder.setLmz(mz1-diff1).setHmz(mz1+diff1))
					.setFragment(Bounds.newBuilder.setLmz(mz2-diff2).setHmz(mz2+diff2))
				)
		}
		MasterRequest.newBuilder.setGetTracesFor(req).build
	}
	
	def randMzAndDiff = {
		val mz = 400.0 + scala.util.Random.nextDouble*800.0
		val diff = mz * params.tracePPM / 1e6
		(mz, diff)
	} 
}