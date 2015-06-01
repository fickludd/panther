package se.lth.immun

case class DataSpectrum(time:Double, mz:Seq[Double], intensity:Seq[Double]) {
	def sum(lowMz:Double, highMz:Double):Double = {
		val i0 = mz.indexWhere(_ > lowMz) // first index where predicate hold
		val in = mz.indexWhere(_ > highMz, i0) // same, starting from i0 
		intensity.slice(i0, in).sum // slice: subsequence by indices.
	}
}
case class DataTrace(time:Seq[Double], intensity:Seq[Double])


object DataStore {
	trait Status
	case object UnInitialized extends Status
	case object Ready extends Status
	case class Loading(loaded:Int, total:Int) extends Status
}


trait DataStore {
	def extractL1Trace(
			lowMz:Double, 
			highMz:Double
		):DataTrace
	
	def extractL2Trace(
			precLowMz:Double, 
			precHighMz:Double, 
			fragLowMz:Double, 
			fragHighMz:Double
		):DataTrace
		
	def status:DataStore.Status
}

trait DataStorer {
	def setNumSpectra(n:Int):Unit
	def addL1Spectrum(ds:DataSpectrum):Unit
	def addL2Spectrum(key:Double, ds:DataSpectrum):Unit
}

