package se.lth.immun

import se.jt.CLIApp
import java.util.Properties

import akka.actor._

object CatSight extends CLIApp {

	val params = new CatSightParams
	
	var properties = new Properties
	properties.load(this.getClass.getResourceAsStream("/pom.properties"))
	val name 		= properties.getProperty("pom.artifactId")
	val version 	= properties.getProperty("pom.version")
	
	
	def main(args:Array[String]) = {
		failOnError(parseArgs(name, version, args, params, List(), None))
		failOnError(parseParams(params.loadAssayConf, params.assayConf.value))
		
		val system = ActorSystem()
		
		val swing = system.actorOf(
				SwingActor.props(params).withDispatcher("swing-dispatcher"), 
				"swing-actor"
			)
			
		val initSourceDefs = params.parseSources.toSeq
		if (initSourceDefs.nonEmpty) 
			swing ! SwingActor.InitSourceDefs(initSourceDefs)
			
		swing ! params.loadAssayConf
		
		for (traML <- params.traMLOpt)
			swing ! SwingActor.InitTraML(traML)
		
		system.awaitTermination
		println("done")
	}
}