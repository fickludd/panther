package se.lth.immun

import java.io.File
import java.io.FileReader
import java.io.BufferedReader
import java.util.Properties
import java.net.InetSocketAddress

import se.jt.CLIApp
import se.lth.immun.xml.XmlReader
import se.lth.immun.mzml._
import se.lth.immun.mzml.ghost._

import se.lth.immun.protocol.MSDataProtocolActors

import scala.collection.mutable.HashMap

import akka.actor._

object Panther extends CLIApp {
	
		
	val params = new PantherParams
	val ds = new SimpleDataStore
	
	var properties = new Properties
	properties.load(this.getClass.getResourceAsStream("/pom.properties"))
	val name 		= properties.getProperty("pom.artifactId")
	val version 	= properties.getProperty("pom.version")
	
	def main(args:Array[String]) = {
		
		failOnError(parseArgs(name, version, args, params, List("mzML"), None))
		
		
		val system = ActorSystem()
		val localAddress = new InetSocketAddress("localhost", 12345)
		
		val logger = system.actorOf(Props[Logger])
		val server = system.actorOf(MSDataProtocolActors.Server.props(
				localAddress, 
				logger, 
				() => system.actorOf(RequestHandler.props(ds))
			), name = "server")
		
		val r = new XmlReader(new BufferedReader(new FileReader(new File(params.mzML))))
		
		
		if (params.mockBig)
			Mock.big(ds)
		else
			MzMLReader.parseMzML(r, ds, params)
			
		println("READY!")
		
		system.awaitTermination
		println("done")
	}
	
}