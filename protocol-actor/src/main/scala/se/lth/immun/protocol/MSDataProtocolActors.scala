package se.lth.immun.protocol

import java.net.InetSocketAddress
import akka.actor._
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import Tcp._


object MSDataProtocolActors {
	
	trait MSDataProtocolMsg
	case class MSDataProtocolError(msg:String, address:InetSocketAddress) extends MSDataProtocolMsg
	case class MSDataProtocolStatus(msg:String, address:InetSocketAddress) extends MSDataProtocolMsg
	case class MSDataReply(msg:MSDataProtocol.MasterReply, nBytes:Int, checkSum:Long, timeTaken:Long, address:InetSocketAddress) extends MSDataProtocolMsg
	case class MSDataProtocolConnected(remote:InetSocketAddress, local:InetSocketAddress) extends MSDataProtocolMsg
	
	object ClientInitiator {
		def props(remote: InetSocketAddress, replies: ActorRef) =
			Props(classOf[ClientInitiator], remote, replies)
	}

	class ClientInitiator(remote: InetSocketAddress, customer: ActorRef) extends Actor {

		import context.system

		IO(Tcp) ! Connect(remote)

		def receive = {
			case CommandFailed(_: Connect) =>
				customer ! MSDataProtocolError("connect failed", remote)
				context stop self

			case c @ Connected(remote, local) =>
				val connection = sender()
				val manager = context.actorOf(ClientManager.props(customer, connection))
				connection ! Register(manager)
				manager ! c
		}
	}
	
	
	object ClientManager {
		def props(customer:ActorRef, connection:ActorRef) =
			Props(classOf[ClientManager], customer, connection)
	}
	class ClientManager(customer:ActorRef, connection:ActorRef) extends Actor {
		
		var sendTime:Long = 0
		var repSize:Int = 0
		var dataBuffer = ByteString()
		var remote:InetSocketAddress = _
		
		import MSDataProtocol._
		
		def receive = {
			case c @ Connected(remote, local) =>
				this.remote = remote
				customer ! MSDataProtocolConnected(remote, local)
			
			case req:MasterRequest =>
				//println("CLIENT: wrote %d bytes".format(data.length))
				connection ! Write(ByteString() ++ req.toByteArray)
				sendTime = System.currentTimeMillis
			
			case reqBuider:MasterRequest.Builder =>
				//println("CLIENT: wrote %d bytes".format(data.length))
				connection ! Write(ByteString() ++ reqBuider.build.toByteArray)
				sendTime = System.currentTimeMillis
				
			case CommandFailed(w: Write) => // O/S buffer was full
				customer ! MSDataProtocolError("cmd failed", remote)
				
			case Received(data) =>
				if (repSize == 0) {
					val sizeMsg = ReplySize.parseFrom(data.take(5).toArray)
					repSize = sizeMsg.getSize
					dataBuffer = data.drop(5)
				} else 
					dataBuffer = dataBuffer ++ data
				
				if (dataBuffer.length == repSize) {
					val msg = MasterReply.parseFrom(dataBuffer.toArray)
					customer ! MSDataReply(
							msg, 
							dataBuffer.length, 
							dataBuffer.map(_.toLong).sum, 
							System.currentTimeMillis - sendTime,
							remote
						)
					repSize = 0
				}
				
			case "close" =>
				connection ! Close
			
			case _: ConnectionClosed =>
				customer ! MSDataProtocolStatus("connection closed", remote)
				context stop self
		}
	}
	
	
	
	object Server {
		def props(
				incoming:InetSocketAddress, 
				logger:ActorRef, 
				reqHandler:() => ActorRef
		) = Props(classOf[Server], incoming, logger, reqHandler)
	}
	
	class Server(
			incoming:InetSocketAddress, 
			logger:ActorRef, 
			reqHandler:() => ActorRef
	) extends Actor {
	
		import context.system
	
		IO(Tcp) ! Bind(self, incoming)
	
		def receive = {
			case b @ Bound(localAddress) =>
				logger ! MSDataProtocolStatus("bound to "+localAddress, localAddress)
	
			case CommandFailed(_: Bind) => 
				logger ! MSDataProtocolError("failed to bind to "+incoming, incoming)
				context stop self
	
			case c @ Connected(remote, local) =>
				logger ! MSDataProtocolStatus("handling connection from "+remote, remote)
				sender() ! Register(reqHandler())
		}
	
	}
	
}