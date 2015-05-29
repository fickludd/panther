package se.lth.immun

import akka.actor._
import akka.actor.{ Actor, ActorRef, Props }
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import akka.util.CompactByteString
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import se.lth.immun.protocol.MSDataProtocol._
import Tcp._

import scala.util.Random

object Kitten {

	val PPM = 20
	
	def main(args: Array[String]) = {
		val system = ActorSystem()
		val kittenListener = system.actorOf(Props[Listener])
		val kitten = system.actorOf(Client.props(new InetSocketAddress("localhost", 12345), kittenListener), name = "client")
		system.awaitTermination
	}

	object Client {
		def props(remote: InetSocketAddress, replies: ActorRef) =
			Props(classOf[Client], remote, replies)
	}

	class Client(remote: InetSocketAddress, listener: ActorRef) extends Actor {

		import context.system

		IO(Tcp) ! Connect(remote)

		def receive = {
			case CommandFailed(_: Connect) =>
				listener ! "connect failed"
				context stop self

			case c @ Connected(remote, local) =>
				println("connected to " + remote)
				val connection = sender()
				connection ! Register(self)
				context become {
					case data: ByteString =>
						//println("CLIENT: wrote %d bytes".format(data.length))
						connection ! Write(data)
					case CommandFailed(w: Write) =>
						println("CLIENT: cmd failed")
						// O/S buffer was full
						listener ! "write failed"
					case Received(data) =>
						//println("CLIENT: got %d bytes".format(data.length))
						listener ! data
					case "close" =>
						println("CLIENT: closing")
						connection ! Close
					case _: ConnectionClosed =>
						listener ! "connection closed"
						context stop self
				}
				listener ! c
		}
	}

	class Listener extends Actor {

		var sendTime:Long = _
		
		var repSize:Int = 0
		var dataBuffer = ByteString()
		
		def receive = {
			case msg: String =>
				println(msg)

			case c @ Connected(remote, local) =>
				sendRand
 
			case data: ByteString =>
				//println(data.decodeString("utf-8"))
				
				if (repSize == 0) {
					val sizeMsg = ReplySize.parseFrom(data.take(5).toArray)
					repSize = sizeMsg.getSize
					dataBuffer = data.drop(5)
				} else 
					dataBuffer = dataBuffer ++ data
				
				if (dataBuffer.length == repSize) {
					val msg = MasterReply.parseFrom(dataBuffer.toArray)
					println("CLIENT: parsed %d bytes: %d".format(dataBuffer.length, dataBuffer.map(_.toLong).sum))
					println("round time %d ms".format(System.currentTimeMillis - sendTime))
					repSize = 0
					sendRand
				}
		}
		
		def sendRand = {
			sendTime = System.currentTimeMillis
			sender ! reqRandAssay(3, 6)
		}
		
		
		
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
			ByteString() ++ MasterRequest.newBuilder.setGetTracesFor(req).build.toByteArray
		}
		
		def randMzAndDiff = {
			val mz = 400.0 + Random.nextDouble*800.0
			val diff = mz * PPM / 1e6
			(mz, diff)
		} 

		
		
		def reqPrecursor(mz:Double, mzWidth:Double) = {
			val req = GetTracesFor.newBuilder()
				.addPrecursor(Bounds.newBuilder().setLmz(mz).setHmz(mz + mzWidth))

			ByteString() ++ MasterRequest.newBuilder.setGetTracesFor(req).build.toByteArray
		}

		
		
		def reqFragment(precMz:Double, fragMz:Double, mzWidth:Double) = {
			val req = GetTracesFor.newBuilder()
				.addFragment(
					FragmentBounds.newBuilder()
						.setPrecursor(
							Bounds.newBuilder().setLmz(precMz).setHmz(precMz + mzWidth))
						.setFragment(
							Bounds.newBuilder().setLmz(fragMz).setHmz(fragMz + mzWidth)))

			ByteString() ++ MasterRequest.newBuilder.setGetTracesFor(req).build.toByteArray
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