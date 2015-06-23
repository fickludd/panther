package se.lth.immun.protocol

import java.net.InetSocketAddress
import akka.actor._
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import Tcp._


object MSDataProtocolActors {
	
	
	val SIZE_MSG_SIZE = 5
	
	trait MSDataProtocolMsg
	case class MSDataProtocolError(msg:String, address:InetSocketAddress) extends MSDataProtocolMsg
	case class MSDataProtocolStatus(msg:String, address:InetSocketAddress) extends MSDataProtocolMsg
	case class MSDataReply(msg:MSDataProtocol.MasterReply, nBytes:Int, checkSum:Long, timeTaken:Long, address:InetSocketAddress) extends MSDataProtocolMsg
	case class MSDataRequest(msg:MSDataProtocol.MasterRequest, nBytes:Int, checkSum:Long, address:InetSocketAddress) extends MSDataProtocolMsg
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
	class ClientManager(
			customer:ActorRef, 
			connection:ActorRef
	) extends MSDataConnectionManager(customer, connection) {
		
		import MSDataProtocol._
		
		def parseIncoming(bytes:Array[Byte]):Any = 
			MSDataReply(
				MasterReply.parseFrom(bytes.toArray), 
				bytes.length, 
				bytes.map(_.toLong).sum, 
				System.currentTimeMillis - sendTime,
				remote
			)
		
		def makeOutMessage(x:Any):Array[Byte] =
			x match {
				case req:MasterRequest => req.toByteArray
				case reqBuider:MasterRequest.Builder => reqBuider.build.toByteArray
			}
	}
	
	
	
	object ServerInitiator {
		def props(
				incoming:InetSocketAddress, 
				logger:ActorRef, 
				reqHandler:() => ActorRef
		) = Props(classOf[ServerInitiator], incoming, logger, reqHandler)
	}
	
	class ServerInitiator(
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
				val connection = sender
				val manager = context.actorOf(ServerManager.props(reqHandler(), connection))
				connection ! Register(manager)
				manager ! c
		}
	}
	
	
	
	object ServerManager {
		def props(reqHandler:ActorRef, connection:ActorRef) =
			Props(classOf[ServerManager], reqHandler, connection)
	}
	class ServerManager(
			reqHandler:ActorRef, 
			connection:ActorRef
	) extends MSDataConnectionManager(reqHandler, connection) {
		
		import MSDataProtocol._
		
		def parseIncoming(bytes:Array[Byte]):Any = 
			MSDataRequest(MasterRequest.parseFrom(bytes), bytes.length, bytes.map(_.toLong).sum, remote)
		
		def makeOutMessage(x:Any):Array[Byte] =
			x match {
				case reply:MasterReply => reply.toByteArray
				case replyBuider:MasterReply.Builder => replyBuider.build.toByteArray
			}
	}
	
	
	abstract class MSDataConnectionManager(downStream:ActorRef, connection:ActorRef) extends Actor {
		var inSize:Int = 0
		var dataBuffer = ByteString()
		var remote:InetSocketAddress = _
		var sendTime = 0L
		
		import MSDataProtocol._
		
		def receive = {
			case c @ Connected(remote, local) =>
				this.remote = remote
				downStream ! MSDataProtocolConnected(remote, local)
				
			case CommandFailed(w: Write) => // O/S buffer was full
				downStream ! MSDataProtocolError("cmd failed", remote)
				
			case Received(data) =>
				dataBuffer = dataBuffer ++ data
				if (inSize == 0 && dataBuffer.length >= SIZE_MSG_SIZE) {
					val sizeMsg = MsgSize.parseFrom(dataBuffer.take(SIZE_MSG_SIZE).toArray)
					inSize = sizeMsg.getSize
					dataBuffer = dataBuffer.drop(5)
				}
				
				if (dataBuffer.length >= inSize) {
					downStream ! parseIncoming(dataBuffer.take(inSize).toArray)
					dataBuffer = dataBuffer.drop(inSize)
					inSize = 0
				}
			
			case _: ConnectionClosed =>
				downStream ! MSDataProtocolStatus("connection closed", remote)
				context stop self
				
			case "close" =>
				connection ! Close
				
			case x =>
				connection ! Write(prependSize(makeOutMessage(x)))
				sendTime = System.currentTimeMillis
		}
		
		def parseIncoming(bytes:Array[Byte]):Any
		
		def makeOutMessage(x:Any):Array[Byte]
		
		def prependSize(bytes:Array[Byte]):ByteString = {
			val sizeMsg = MsgSize.newBuilder.setSize(bytes.length)
			val headBytes = sizeMsg.build.toByteArray
			ByteString() ++ headBytes ++ bytes
		}
	}
	
}