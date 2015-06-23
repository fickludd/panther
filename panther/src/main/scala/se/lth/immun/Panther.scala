package se.lth.immun

import java.io.File
import java.io.FileReader
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
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
		
		val address = params.parseAddress
		
		println("  %s %s".format(name, version))
		println("binding to "+address)
		
		val logger = system.actorOf(Props[Logger])
		val server = system.actorOf(MSDataProtocolActors.ServerInitiator.props(
				address, 
				logger, 
				() => system.actorOf(RequestHandler.props(ds))
			), name = "server")
		
		
		if (params.mockBig)
			Mock.big(ds)
		else
			MzMLReader.parseMzML(getReader(params.mzML), ds, params)
			
		println("READY!")
		
		system.awaitTermination
		println("done")
	}
	
	
	def getReader(path:String):XmlReader = {
		val f = new File(path)
		if (path.toLowerCase.endsWith(".mzml.gz"))
			new XmlReader(new BufferedReader(new InputStreamReader(
							new GZIPInputStream(new FileInputStream(f)))))
		else if (path.toLowerCase.endsWith(".mzml"))
			new XmlReader(new BufferedReader(new FileReader(f)))
		else
			throw new Exception("Unknown file format '%s'".format(path))
	}
	
}