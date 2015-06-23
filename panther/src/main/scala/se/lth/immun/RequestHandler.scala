package se.lth.immun

import akka.actor._
import akka.io.Tcp
import akka.util.ByteString

import se.lth.immun.protocol.MSDataProtocol
import se.lth.immun.protocol.MSDataProtocolActors._

import scala.collection.JavaConversions._

object RequestHandler {
	def props(dataStore: DataStore) =
		Props(classOf[RequestHandler], dataStore)
}

class RequestHandler(val dataStore: DataStore) extends Actor {
	import MSDataProtocol._
	import Tcp._
	def receive = {
		case MSDataRequest(req, nBytes, checkSum, remote) =>
			val reply = MasterReply.newBuilder
			reply.setId(req.getId)
			if (req.hasGetStatus)
				replyToGetStatus(reply)
			if (req.hasGetTracesFor)
				replyToGetTracesFor(reply, req.getGetTracesFor)
			
			sender ! reply.build
		

		case PeerClosed =>
			println("HANDLER: peer closed ")
			context stop self

		case x =>
			println("REQ_HANDLER: "+x)
	}
	
	
	
	def replyToGetStatus(reply:MasterReply.Builder) = {
		val status = MSDataProtocol.Status.newBuilder
		dataStore.status match {
			case DataStore.Ready =>
				status.setStatus(MSDataProtocol.Status.StatusType.up)
			
			case DataStore.Loading(loaded, total) =>
				status.setStatus(MSDataProtocol.Status.StatusType.loadingMzML)
				status.setProgress(loaded)
				status.setProgressMax(total)
				
			case DataStore.UnInitialized =>
				status.setStatus(MSDataProtocol.Status.StatusType.loadingMzML)
				status.setProgress(0.0)
				status.setProgressMax(1.0)
				
		}
		reply.setStatus(status)
	}
	
	
	
	def replyToGetTracesFor(reply:MasterReply.Builder, req:GetTracesFor) = {
		val traces = Traces.newBuilder

		for (prec <- req.getPrecursorList) {
			val dataTrace = dataStore.extractL1Trace(prec.getLmz, prec.getHmz)
			val trace = Trace.newBuilder()
			for (t <- dataTrace.time) trace.addTime(t)
			for (int <- dataTrace.intensity) trace.addIntensity(int)
			traces.addPrecursor(
				PrecursorTrace.newBuilder
					.setPrecursor(prec)
					.setTrace(trace))
		}

		for (frag <- req.getFragmentList) {
			val dataTrace = dataStore.extractL2Trace(
				frag.getPrecursor.getLmz, frag.getPrecursor.getHmz,
				frag.getFragment.getLmz, frag.getFragment.getHmz)
			val trace = Trace.newBuilder()
			for (t <- dataTrace.time) trace.addTime(t)
			for (int <- dataTrace.intensity) trace.addIntensity(int)
			traces.addFragment(
				FragmentTrace.newBuilder
					.setFragment(frag)
					.setTrace(trace))
		}
		
		reply.setTraces(traces)
	}
}