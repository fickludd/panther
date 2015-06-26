package se.lth.immun

import scala.collection.mutable.HashMap

class SimpleDataStore extends DataStore with DataStorer {

	import DataStorer._
	
	val dmLevel1 = new DataMatrix
	val dmLevel2 = new HashMap[PrecDef, DataMatrix]
	var totalSpectra = 0
	
	def extractL1Trace(
			lowMz:Double, 
			highMz:Double
	):DataTrace = {
		DataTrace(dmLevel1.times, dmLevel1.trace(lowMz, highMz))
	}
	
	def extractL2Trace(
			precLowMz:Double, 
			precHighMz:Double, 
			fragLowMz:Double, 
			fragHighMz:Double
	):DataTrace = {
		val dmKey = dmLevel2.keys.maxBy(overlap(precLowMz, precHighMz))
		val dm = dmLevel2(dmKey)
		DataTrace(dm.times, dm.trace(fragLowMz, fragHighMz))
	}
	
	def addL1Spectrum(ds:DataSpectrum) = {
		dmLevel1.spectra += ds
	}
	
	def addL2Spectrum(key:PrecDef, ds:DataSpectrum) = {
		if (!dmLevel2.contains(key))
			dmLevel2 += key -> new DataMatrix
		dmLevel2(key).spectra += ds
	}
	
	def status =
		if (totalSpectra <= 0) DataStore.UnInitialized
		else {
			val loadedSpectra = dmLevel1.spectra.length + dmLevel2.map(_._2.spectra.length).sum
			if (loadedSpectra == totalSpectra)
				DataStore.Loading(loadedSpectra, totalSpectra)
			else
				DataStore.Ready
		}
	
	def setNumSpectra(n:Int):Unit = 
		totalSpectra = n
		
	def overlap(precLowMz:Double, precHighMz:Double)(precDef:PrecDef) =
		precDef.map(r => math.max(0.0, math.min(r.high, precHighMz) - math.max(r.low, precLowMz))).sum
}