package se.lth.immun

import se.jt.Params
import java.io.File
import scala.io.Source


class CatSightParams extends Params {

	import Params._
	
	val tracePPM = 20.0			## "extraction window of either side of target trace mass"
	val sources = ""			## "File with source to connect to on startup (1 source per row, name-tab-ip:port)"
	val traML = ""				## "Traml to load on startup"
	val assayConf = ""			## "File defining assay load options. set to HELP for help"

	val sourceRE = """(.+)\t(.+):(\d+)""".r
	def parseSource(str:String) =
		str match {
			case sourceRE(name, ip, port) =>
				Some(SourceDef(ip, port.toInt, name))
			case _ => None
		}
	
	def parseSources:Iterator[SourceDef] = 
		if (sources.value == "") Nil.toIterator
		else
			for {
				line <- Source.fromFile(new File(sources)).getLines
				source <- parseSource(line)
			} yield source

	def traMLOpt = 
		if (traML.value == "") None
		else Some(traML.value)
			
	val loadAssayConf = new LoadAssayParams
	val outSep = "\t"
	val outQuote = "\""
}