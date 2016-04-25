package se.lth.immun

import scala.collection.mutable.HashMap

class FlexibleDataStore(params:PantherParams) extends SimpleDataStore(params) {

	import DataStorer._
	
	override def extractL2Trace(
			precLowMz:Double, 
			precHighMz:Double, 
			fragLowMz:Double, 
			fragHighMz:Double
	):DataTrace = {
		val dmKeys = dmLevel2.keys.filter(0 < overlap(precLowMz, precHighMz)(_))
		if (params.verbose)
			println("selected swaths "+dmKeys.mkString(";")+" for precursor %.4f-%.4f".format(precLowMz, precHighMz))
		val traces = 
			dmKeys.map(key => {
				val dm = dmLevel2(key)
				DataTrace(dm.times, dm.trace(fragLowMz, fragHighMz))
			}).toSeq
		mergeTraces(traces)
	}
	
	def mergeTraces(traces:Seq[DataTrace]):DataTrace = {
		val ls = traces.map(_.time.length)
		val times = new Array[Float](ls.sum)
		val intensities = new Array[Float](ls.sum)
		val indices = traces.map(_ => 0).toArray
		
		def nextSourceTrace:Int = 
			(0 until ls.length)
				.filter(i => indices(i) < ls(i))
				.minBy(i => traces(i).time(indices(i)))
		
		for (targetIndex <- 0 until ls.sum) {
			val iTrace = nextSourceTrace
			times(targetIndex) = traces(iTrace).time(indices(iTrace))
			intensities(targetIndex) = traces(iTrace).intensity(indices(iTrace))
			indices(iTrace) += 1
		}
		DataTrace(times, intensities)
	}
}