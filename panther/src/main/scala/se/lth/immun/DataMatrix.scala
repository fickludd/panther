package se.lth.immun

import scala.collection.mutable.ArrayBuffer

class DataMatrix {

	val spectra = new ArrayBuffer[DataSpectrum]
	lazy val times = spectra.map(_.time).toArray
	
	def trace(lowMz:Double, highMz:Double) =
		spectra.map(_.sum(lowMz, highMz))
}
