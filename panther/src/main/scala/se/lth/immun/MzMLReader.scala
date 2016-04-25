package se.lth.immun

import se.jt.CLIBar
import se.lth.immun.xml.XmlReader
import se.lth.immun.mzml._
import se.lth.immun.mzml.ghost._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

import scala.collection.mutable.Queue

object MzMLReader {
	
	val ISOLATION_WINDOW_TARGET = "MS:1000827"
	val ISOLATION_WINDOW_LOWER_OFF = "MS:1000828"
	val ISOLATION_WINDOW_UPPER_OFF = "MS:1000829"
	val SELECTED_ION_MZ_ACC = "MS:1000744"
		
	import DataStorer._
		
	def parseMzML(r:XmlReader, ds:DataStorer, params:PantherParams) = {
		MzML.fromFile(r, new MzMLDataHandlers(
			setupDataStructures(ds, params),
			handleSpectrum(ds, params),
			(i:Int) => _,
			(c:Chromatogram) => _
		))
		processQueue(ds, specQueue.length)
	}
	
	
	def setupDataStructures(ds:DataStorer, params:PantherParams)(n:Int) = {
		println("this file contains %d spectra".format(n))
		ds.setNumSpectra(n)
		totalSpecs = n
		println(bar.reference)
		print(bar.update(0, totalSpecs))
	}
	
	val t0 = System.currentTimeMillis
	val specQueue = new Queue[Future[(GhostSpectrum, DataSpectrum)]]
	val bar = new CLIBar
	var totalSpecs = 0
	var handledSpecs = 0
	
	def handleSpectrum(ds:DataStorer, params:PantherParams)(s:Spectrum):Unit = {
		if (params.startSpectrumIndex < params.lastSpectrumIndex &&
				(s.index < params.startSpectrumIndex || 
				s.index > params.lastSpectrumIndex))
			return
		
		if (params.verbose) {
			if (s.index % params.verboseFreq == 0) 
				println("at spectrum %8d %8ds   %d / %d Mb".format(
					s.index,
					(System.currentTimeMillis - t0) / 1000, 
					Runtime.getRuntime().totalMemory() / 1000000, 
					Runtime.getRuntime().maxMemory() / 1000000))
		} else {
			handledSpecs += 1
			print(bar.update(handledSpecs, totalSpecs))
		}
		
		specQueue += Future {
				val gs = GhostSpectrum.fromSpectrum(s)
				val mzs = gs.mzs.toArray
				val ints = gs.intensities.toArray
				
				val nNoZero = ints.count(_ > 0)
				val intsWithout0 = new Array[Float](nNoZero)
				val mzsWithout0 = new Array[Double](nNoZero)
				var k = 0
				for (j <- 0 until mzs.length)
					if (ints(j) != 0) {
						intsWithout0(k) = ints(j).toFloat
						mzsWithout0(k) 	= mzs(j)
						k+=1
					}
				(gs, DataSpectrum(gs.scanStartTime.toFloat, mzsWithout0, intsWithout0))
			}
		
		if (specQueue.length > params.specQueueSize)
			processQueue(ds:DataStorer, specQueue.length - params.specQueueSize)

	}
	
	
	def processQueue(ds:DataStorer, n:Int) = {
		for (i <- 0 until n) {
			val fds = specQueue.dequeue
			val (gs, dataSpectrum) = Await.result(fds, Duration.Inf)
			gs.msLevel match {
				case GhostSpectrum.MS1 => ds.addL1Spectrum(dataSpectrum)
				case GhostSpectrum.MS2(precMz, isoWindow) =>
					isoWindow.map(iw => Set(MzRange(iw.low, iw.high))).orElse(getLevel2Key(gs)) match {
						case Some(key) =>
							ds.addL2Spectrum(key, dataSpectrum)
						case None => {}
					}
				case _ => throw new Exception("unhandlable ms level "+gs.msLevel)
			}
		}
	}
	
	
	def getLevel2Key(gs:GhostSpectrum):Option[PrecDef] = {
		val iws = gs.spectrum.precursors.flatMap(_.isolationWindow)
		val sis = gs.spectrum.precursors.flatMap(_.selectedIons)
		val iwDef = iws.map(isolationWindow2Range).toSet
		val siDef = sis.map(trippleTOFselectedIon2Range).toSet
		if (iwDef.isEmpty && siDef.isEmpty) None
		else if (iwDef.nonEmpty) Some(iwDef)
		else Some(siDef)
	}
	
	def isolationWindow2Range(iw:IsolationWindow):MzRange = {
		val iwTarget = iw.cvParams.find(_.accession == ISOLATION_WINDOW_TARGET)
		val iwLower = iw.cvParams.find(_.accession == ISOLATION_WINDOW_LOWER_OFF)
		val iwUpper = iw.cvParams.find(_.accession == ISOLATION_WINDOW_UPPER_OFF)
		
		(iwTarget, iwLower, iwUpper) match {
			case (Some(iwt), Some(iwl), Some(iwu)) =>
				val tmz = iwt.value.get.toDouble
				val lmz = iwl.value.get.toDouble
				val umz = iwu.value.get.toDouble
				MzRange(tmz-lmz, tmz+umz)
			case _ =>
				throw new Exception("Erroneously defined isolation window: target=%s, lower=%s, upper=%s".format(iwTarget, iwLower, iwUpper))
				
		}
	}
	
	def trippleTOFselectedIon2Range(si:SelectedIon):MzRange = {
		val swathCenter = si.cvParams.find(_.accession == SELECTED_ION_MZ_ACC)
		swathCenter match {
			case Some(cv) =>
				val mz = cv.value.get.toDouble
				MzRange(mz-12.5, mz+12.5)
			case None =>
				throw new Exception("Erroneously defined selected ion: NO SELECTED ION MZ")
		}
	}
}