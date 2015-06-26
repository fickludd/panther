package se.lth.immun

import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Success, Failure}

import java.io.File
import java.io.FileReader
import java.io.BufferedReader


import se.lth.immun.xml.XmlReader
import se.lth.immun.traml.ghost._

class AddAssaysDialog(
		onClose:Seq[Assay] => Unit,
		params:LoadAssayParams,
		traMLOpt:Option[String] = None
) extends Dialog {
	
	
	val addTraml = Button("traml") { 
		val tramlChooser = new FileChooser(new File("."))
		tramlChooser.showOpenDialog(assayText)
		if (tramlChooser.selectedFile != null)
			readTraml(tramlChooser.selectedFile)
	}
	val addTextFile = Button("csv/tsv") { 
		val fileChooser = new FileChooser(new File("."))
		fileChooser.showOpenDialog(assayText)
		if (fileChooser.selectedFile != null)
			readTextFile(fileChooser.selectedFile)
	}
	val splitPrecAndFrag = new CheckBox { text = "split prec/frag" }
	val fragFilterRE = new TextField { text = params.fragFilterRE }
	val fragSubGroupRE = new TextField { text = params.fragSubGroupRE }
	val precSubGroupRE = new TextField { text = params.precSubGroupRE }
	val cancel = Button("cancel") { 
		onClose(Nil)
		close
	}
	val ok = Button("ok") {
		parseAssayText(assayText.text) match {
			case Success(assays) =>
				onClose(assays)
				close
			case Failure(e) =>
				errorFeedback.text = e.getMessage
		}
	}
	
	val errorFeedback = new Label
	val assayText = new TextArea
	preferredSize = new Dimension(800, 600)
	
	for (traML <- traMLOpt)
		readTraml(new File(traML))
	
	ok.requestFocusInWindow
	
	contents = new BorderPanel {
		val bottomGrid = new GridPanel(4, 1) {
			contents += errorFeedback
			contents += new GridPanel(1,3) {
				contents += splitPrecAndFrag
				contents += new Label { text = "prec subgroup regex" }
				contents += precSubGroupRE
			}
			contents += new GridPanel(1,4) {
				contents += new Label { text = "frag filter regex" }
				contents += fragFilterRE
				contents += new Label { text = "frag subgroup regex" }
				contents += fragSubGroupRE
			}
			contents += new GridPanel(1,4) {
				contents += addTraml
				contents += addTextFile
				contents += cancel
				contents += ok
			}
		}
		val scroller = new ScrollPane(assayText)
		
		layout(scroller) = Center
		layout(bottomGrid) = South
	}
	
	
	def readTextFile(f:File) = {
		val r = new BufferedReader(new FileReader(f))
		val sb = new StringBuilder
		var line = r.readLine()
		while (line != null) {
			sb ++= line
			line = r.readLine()
		}
		r.close
		assayText.text = sb.result
		
	}
	
	
	def readTraml(f:File) = {
		
		def makeIDfromTransition(gt:GhostTransition) =
			(gt.compoundRef, gt.peptideRef, gt.q1z)
		def makeIDfromTarget(gt:GhostTarget) =
			(gt.compoundRef, gt.peptideRef, gt.q1z)
		def clean(str:String) =
			if (str == null) "" else str
			
		val r = new XmlReader(new BufferedReader(new FileReader(f)))
		val traml = GhostTraML.fromFile(r)
		val sb = new StringBuilder
		sb ++= "z\tpeptide\tcompound\tirt\tprecMz\tfragMz\ttraceId\tfragIntensity\n"
		for (((pep, comp, z), gts) <- traml.transitions.groupBy(makeIDfromTransition)) {
			val pepHead = z + "\t" + clean(pep) + "\t" + clean(comp)
			for (gt <- gts) {
				sb ++= pepHead + "\t" + gt.irt + "\t" + gt.q1 + "\t" + gt.q3 + "\t" + gt.id + "\t" + gt.intensity + "\n"
			}
		}
		for (((pep, comp, z), gts) <- traml.includes.groupBy(makeIDfromTarget)) {
			val pepHead = z + "\t" + clean(pep) + "\t" + clean(comp)
			for (gt <- gts) {
				sb ++= pepHead + "\t" + "" + "\t" + gt.q1 + "\t" + "" + "\t" + gt.id + "\t" + gt.intensity + "\n"
			}
		}
		assayText.text = sb.result
	}
	
	
	val SEPS = Array('\t', ',', ';', ' ')
	def parseAssayText(text:String):Try[Seq[Assay]] = {
		
		def without(str:String, prefix:String) =
			if (str.startsWith(prefix)) str.drop(prefix.length)
			else str
		
		val sep = SEPS.maxBy(s => text.take(1000).count(_ == s))
		
		val lines = text.split("\n")
		val cols 		= lines.head.split(sep).map(_.trim.toLowerCase)
		val iASSAY_ID 	= cols.indexWhere(_ == "assayid")
		val iZ 			= cols.indexWhere(Array("z", "charge").contains)
		val iPEPTIDE 	= cols.indexWhere(_ == "peptide")
		val iCOMPOUND 	= cols.indexWhere(_ == "compound")
		val iIRT 		= cols.indexWhere(_ == "irt")
		val iPREC_MZ 	= cols.indexWhere(_ == "precmz")
		val iFRAG_MZ 	= cols.indexWhere(_ == "fragmz")
		val iTRACE_ID 	= cols.indexWhere(_ == "traceid")
		val iFRAG_INT 	= cols.indexWhere(_ == "fragintensity")
		
		if (iASSAY_ID < 0 && (iZ < 0 || (iPEPTIDE < 0 && iCOMPOUND < 0)))
			return Failure(new Exception("Couldn't find assay id column"))
		
		if (iTRACE_ID < 0)
			return Failure(new Exception("Couldn't find trace id column"))
		
		if (iPREC_MZ < 0)
			return Failure(new Exception("Couldn't find precursor m/z column"))
		
		def get(i:Int, parts:Seq[String]) = 
			if (i < 0 || parts(i) == "") None
			else Some(parts(i))
			
		def getTraceLabel(traceID:String, msLevel:Int) = {
			val levelLabel = 
				if (splitPrecAndFrag.selected && iFRAG_MZ >= 0) 
						"MS"+msLevel
				else ""
			val precLabel = 
				if (msLevel == 1) 
					precSubGroupRE.text match {
						case null 	=> ""
						case "" 	=> ""
						case reStr 	=>
							val r = reStr.r.unanchored
							traceID match {
								case r(label) => label
								case _ => 
									errorFeedback.text = "couldn't match traceID '%s' to regex '%s'".format(traceID, reStr)
									""
							}
					}
				else ""
			val fragLabel = 
				if (msLevel == 2) 
					fragSubGroupRE.text match {
						case null 	=> ""
						case "" 	=> ""
						case reStr 	=>
							val r = reStr.r.unanchored
							traceID match {
								case r(label) => label
								case _ => 
									errorFeedback.text = "couldn't match traceID '%s' to regex '%s'".format(traceID, reStr)
									""
							}
					}
				else ""
			(levelLabel + " " + precLabel + " " + fragLabel).trim
		}
		
		def includeTrace(traceID:String, msLevel:Int) = 
			if (msLevel == 2) 
				fragFilterRE.text match {
					case null 	=> true
					case "" 	=> true
					case reStr 	=>
						val r = reStr.r.unanchored
						r.findFirstIn(traceID) match {
							case Some(str) 	=> false
							case _ 			=> true
						}
				}
			else true
			
		import Assay._
		val assays = new HashMap[String, AssayBuilder]
		for (line <- lines.tail) {
			val parts = line.split(sep).map(_.trim)
			val msLevel = 
				get(iFRAG_MZ, parts) match {
					case None => 1
					case Some(x) => 2
				}
			if (includeTrace(parts(iTRACE_ID), msLevel)) {
				
				val id = 
					if (iASSAY_ID >= 0) parts(iASSAY_ID) 
					else 
						get(iPEPTIDE, parts).getOrElse(get(iCOMPOUND, parts).get)+
							"+".padTo(parts(iZ).toInt, '+') +
						getTraceLabel(parts(iTRACE_ID), msLevel)
					
				val tid = without(
							parts(iTRACE_ID), 
							get(iASSAY_ID, parts)
								.orElse(get(iPEPTIDE, parts))
								.orElse(get(iCOMPOUND, parts)).get) 
							
				if (!assays.contains(id))
					assays += id -> new AssayBuilder
				
				val pmz = parts(iPREC_MZ).toDouble
				Try(parts(iFRAG_MZ).toDouble) match {
					case Success(fmz) =>
						assays(id).frags += Frag(tid, pmz, fmz)
					case Failure(msg) =>
						assays(id).precs += Prec(tid, pmz)
				}
			} 
		}
		
		Success(assays.keys.toSeq.sorted.map(id => new Assay(id, assays(id).precs, assays(id).frags)).toSeq)
	}
}