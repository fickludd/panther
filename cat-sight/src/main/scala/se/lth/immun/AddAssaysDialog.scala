package se.lth.immun

import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._

import scala.collection.mutable.HashMap
import scala.util.{Try, Success, Failure}

import java.io.File
import java.io.FileReader
import java.io.BufferedReader


import se.lth.immun.xml.XmlReader
import se.lth.immun.traml.ghost.GhostTraML

class AddAssaysDialog(
		onClose:Seq[Assay] => Unit
) extends Dialog {
	
	
	val addTraml = Button("traml") { 
		val tramlChooser = new FileChooser(new File("."))
		tramlChooser.showDialog(assayText, null)
		if (tramlChooser.selectedFile != null)
			readTraml(tramlChooser.selectedFile)
	}
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
	
	
	contents = new BorderPanel {
		val bottomGrid = new GridPanel(2, 1) {
			contents += errorFeedback
			contents += new GridPanel(1,3) {
				contents += addTraml
				contents += cancel
				contents += ok
			}
		}
		val scroller = new ScrollPane(assayText)
		
		layout(scroller) = Center
		layout(bottomGrid) = South
	}
	
	
	def readTraml(f:File) = {
		assayText.text = ""
		val r = new XmlReader(new BufferedReader(new FileReader(f)))
		val traml = GhostTraML.fromFile(r)
		val sb = new StringBuilder
		sb ++= "z\tpeptide\tirt\tprecMz\tfragMz\ttraceId\tfragIntensity\n"
		for (((pep, z), gts) <- traml.transitions.groupBy(gt => (gt.peptideRef, gt.q1z))) {
			val pepHead = z + "\t" + pep
			for (gt <- gts) {
				sb ++= pepHead + "\t" + gt.irt + "\t" + gt.q1 + "\t" + gt.q3 + "\t" + gt.id + "\t" + gt.intensity + "\n"
			}
		}
		assayText.text = sb.result
	}
	
	
	val SEPS = Array('\t', ',', ';', ' ')
	def parseAssayText(text:String):Try[Seq[Assay]] = {
		val sep = SEPS.maxBy(s => text.take(1000).count(_ == s))
		
		val lines = text.split("\n")
		val cols 		= lines.head.split(sep).map(_.trim.toLowerCase)
		val iASSAY_ID 	= cols.indexWhere(_ == "assayid")
		val iZ 			= cols.indexWhere(Array("z", "charge").contains)
		val iPEPTIDE 	= cols.indexWhere(_ == "peptide")
		val iIRT 		= cols.indexWhere(_ == "irt")
		val iPREC_MZ 	= cols.indexWhere(_ == "precmz")
		val iFRAG_MZ 	= cols.indexWhere(_ == "fragmz")
		val iTRACE_ID 	= cols.indexWhere(_ == "traceid")
		val iFRAG_INT 	= cols.indexWhere(_ == "fragintensity")
		
		if (iASSAY_ID < 0 && (iZ < 0 || iPEPTIDE < 0))
			return Failure(new Exception("Couldn't find assay id column"))
		
		if (iTRACE_ID < 0)
			return Failure(new Exception("Couldn't find trace id column"))
		
		if (iPREC_MZ < 0)
			return Failure(new Exception("Couldn't find precursor m/z column"))
		
		import Assay._
		val assays = new HashMap[String, AssayBuilder]
		for (line <- lines.tail) {
			val parts = line.split(sep).map(_.trim)
			val id = if (iASSAY_ID >= 0) parts(iASSAY_ID) else parts(iPEPTIDE)+"+".padTo(parts(iZ).toInt, '+')
			val tid = parts(iTRACE_ID) 
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
		
		Success(assays.map(t => new Assay(t._1, t._2.precs, t._2.frags)).toSeq)
	}
}