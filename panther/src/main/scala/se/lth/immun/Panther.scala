package se.lth.immun

import java.io.File
import java.io.FileReader
import java.io.BufferedReader

import se.jt.CLIApp
import se.lth.immun.xml.XmlReader
import se.lth.immun.mzml._
import se.lth.immun.mzml.ghost._

import scala.collection.mutable.HashMap

import akka.actor._

object Panther extends CLIApp {
	
		
	val params = new PantherParams
	val ds = new SimpleDataStore
	
	
	def main(args:Array[String]) = {
		
		failOnError(parseArgs("Panther", "version", args, params, List("mzML"), None))
		
		
		val system = ActorSystem()
		val connRouter = system.actorOf(ConnectionRouter.props(ds), name = "router")
		
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