package se.lth.immun

import se.lth.immun.xml.XmlReader
import se.lth.immun.mzml._
import se.lth.immun.mzml.ghost._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

import scala.collection.mutable.Queue

object MzMLReader {
	
	val ISOLATION_WINDOW_ACC = "MS:1000827"
	val SELECTED_ION_MZ_ACC = "MS:1000744"
		
		
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
	}
	
	val t0 = System.currentTimeMillis
	val specQueue = new Queue[Future[(GhostSpectrum, DataSpectrum)]]
	def handleSpectrum(ds:DataStorer, params:PantherParams)(s:Spectrum):Unit = {
		if (params.startSpectrumIndex < params.lastSpectrumIndex &&
				(s.index < params.startSpectrumIndex || 
				s.index > params.lastSpectrumIndex))
			return
		
		if (s.index % 200 == 0) 
			println("at spectrum %8d %8ds   %d / %d Mb".format(
					s.index,
					(System.currentTimeMillis - t0) / 1000, 
					Runtime.getRuntime().totalMemory() / 1000000, 
					Runtime.getRuntime().maxMemory() / 1000000))
		
		specQueue += Future {
				val gs = GhostSpectrum.fromSpectrum(s)
				val mzs = gs.mzs.toArray
				val ints = gs.intensities.toArray
				
				val nNoZero = ints.count(_ > 0)
				val intsWithout0 = new Array[Double](nNoZero)
				val mzsWithout0 = new Array[Double](nNoZero)
				var k = 0
				for (j <- 0 until mzs.length)
					if (ints(j) != 0) {
						intsWithout0(k)=ints(j)
						mzsWithout0(k)=mzs(j)
						k+=1
					}
				(gs, DataSpectrum(gs.scanStartTime, mzsWithout0, intsWithout0))
			}
		
		if (specQueue.length > params.specQueueSize)
			processQueue(ds:DataStorer, specQueue.length - params.specQueueSize)

	}
	
	
	def processQueue(ds:DataStorer, n:Int) = {
		for (i <- 0 until n) {
			val fds = specQueue.dequeue
			val (gs, dataSpectrum) = Await.result(fds, Duration.Inf)
			if (gs.msLevel == 1)
				ds.addL1Spectrum(dataSpectrum)
				
			else if (gs.msLevel == 2) {
				getLevel2Key(gs) match {
					case Some(key) =>
						ds.addL2Spectrum(key, dataSpectrum)
					case None => {}
				}
				
			} else
				throw new Exception("unhandlable ms level "+gs.msLevel)
		}
	}
	
	
	def getLevel2Key(gs:GhostSpectrum):Option[Double] = {
		val iwOpt = gs.spectrum.precursors.head.isolationWindow
		val siList = gs.spectrum.precursors.head.selectedIons
		iwOpt match {
			case Some(iw) =>
				iw.cvParams.find(_.accession == ISOLATION_WINDOW_ACC) match {
					case Some(cvParam) =>
						Some(cvParam.value.get.toDouble)
					case None =>
						None
				}
			case None =>
				for {
					si <- siList.headOption
					cvParam <- si.cvParams.find(_.accession == "MS:1000744")
				} yield cvParam.value.get.toDouble
		}
		
	}
}