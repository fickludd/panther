package se.lth.immun

import akka.actor._
import akka.actor.{ Actor, ActorRef, Props }
import java.net.InetSocketAddress
import se.lth.immun.protocol.MSDataProtocolActors
import se.lth.immun.protocol.MSDataProtocol._

import scala.collection.JavaConversions._
import scala.util.Random

object Kitten {

	val PPM = 20
	
	def main(args: Array[String]) = {
		val system = ActorSystem()
		//val server = new InetSocketAddress("localhost", 12345)
		val server = new InetSocketAddress("130.235.249.157", 22222)
		val infinitePoller = system.actorOf(Props[InfinitePoller])
		val client = system.actorOf(MSDataProtocolActors.ClientInitiator.props(server, infinitePoller))
		system.awaitTermination
	}

	class InfinitePoller extends Actor {

		import MSDataProtocolActors._
		
		def receive = {
			case msg: String =>
				println(msg)

			case MSDataProtocolConnected(remote, local) =>
				sendRandAssay
 
			case MSDataReply(msg, nBytes, checkSum, timeTaken, remote) =>
				println("KITTEN| parsed %d bytes in %d ms. CHECKSUM=%d".format(nBytes, timeTaken, checkSum))
				//println("First frag prec mz:"+msg.getTraces.getFragmentList.head.getFragment.getPrecursor)
				val (fFloat, fDouble, pFloat, pDouble) = countDataPoints(msg)
				def checkSizes(xs:Seq[(Int, Int)], tag:String) =
					xs.find(t => t._1 != t._2).map(t =>
						println("Different %s time and intensity array lengths: %d vs %d".format(tag, t._1, t._2))
					)
				
				def printIfError(xs:Seq[(Int, Int)]) =
					if (xs.exists(t => t._1 != t._2))
						println(msg)
						
				/*checkSizes(fFloat, "FRAG_FLOAT")
				checkSizes(fDouble, "FRAG_DOUBLE")
				checkSizes(pFloat, "PREC_FLOAT")
				checkSizes(pDouble, "PREC_DOUBLE")
				*/
				printIfError(fDouble)
				sendRandAssay
		}
		
		
		
		def countDataPoints(msg:MasterReply) = {
			val traces = msg.getTraces
			val fl = traces.getFragmentList
			val pl = traces.getPrecursorList
			
			val fragFloatCounts 	= fl.map(f => (f.getSmallTrace.getTimeCount, 	f.getSmallTrace.getIntensityCount))
			val fragDoubleCounts 	= fl.map(f => (f.getTrace.getTimeCount, 		f.getTrace.getIntensityCount))
			val precFloatCounts 	= pl.map(f => (f.getSmallTrace.getTimeCount, 	f.getSmallTrace.getIntensityCount))
			val precDoubleCounts 	= pl.map(f => (f.getTrace.getTimeCount, 		f.getTrace.getIntensityCount))
			(fragFloatCounts, fragDoubleCounts, precFloatCounts, precDoubleCounts)
		}
		
		
		
		def sendRandAssay =
			sender ! reqRandAssay(3, 6)
			//sender ! reqRandAssay(2+Random.nextInt(5), 5+Random.nextInt(30))
		
		
		
		def reqRandAssay(nPrec:Int, nFrag:Int) = {
			val req = GetTracesFor.newBuilder
			for (i <- 0 until nPrec) {
				val (mz, diff) =randMzAndDiff
				req.addPrecursor(Bounds.newBuilder.setLmz(mz-diff).setHmz(mz+diff))
			}
			for (i <- 0 until nFrag) {
				val (mz1, diff1) =randMzAndDiff
				val (mz2, diff2) =randMzAndDiff
				req.addFragment(
					FragmentBounds.newBuilder
						.setPrecursor(Bounds.newBuilder.setLmz(mz1-diff1).setHmz(mz1+diff1))
						.setFragment(Bounds.newBuilder.setLmz(mz2-diff2).setHmz(mz2+diff2))
					)
			}
			req.setTransferMode(TransferMode.DOUBLE)
			MasterRequest.newBuilder.setGetTracesFor(req).build
		}
		
		def randMzAndDiff = {
			val mz = 400.0 + Random.nextDouble*800.0
			val diff = mz * PPM / 1e6
			(mz, diff)
		} 

		
		
		def reqPrecursor(mz:Double, mzWidth:Double) = {
			val req = GetTracesFor.newBuilder()
				.addPrecursor(Bounds.newBuilder().setLmz(mz).setHmz(mz + mzWidth))

			MasterRequest.newBuilder.setGetTracesFor(req).build
		}

		
		
		def reqFragment(precMz:Double, fragMz:Double, mzWidth:Double) = {
			val req = GetTracesFor.newBuilder()
				.addFragment(
					FragmentBounds.newBuilder()
						.setPrecursor(
							Bounds.newBuilder().setLmz(precMz).setHmz(precMz + mzWidth))
						.setFragment(
							Bounds.newBuilder().setLmz(fragMz).setHmz(fragMz + mzWidth)))

			MasterRequest.newBuilder.setGetTracesFor(req).build
		}
		
		
		
		def testMockData = {
			sender ! reqPrecursor(400.05, 0.2)
			sender ! reqPrecursor(510.05, 0.2)
			sender ! reqFragment(512.0, 400.05, 0.3)
			sender ! reqFragment(513.0, 520.05, 0.3)
			sender ! reqFragment(540.0, 400.05, 0.3)
			sender ! reqFragment(540.0, 550.05, 0.3)
		}
	}
}