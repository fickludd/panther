package se.lth.immun

import scala.collection.mutable.ArrayBuffer

class DataMatrix {

	val spectra = new ArrayBuffer[DataSpectrum]
	var _times = Array[Float]()
	def times = {
		if (_times.length != spectra.length)
			_times = spectra.map(_.time).toArray
		_times
	}
	
	def trace(lowMz:Double, highMz:Double) =
		spectra.map(_.sum(lowMz, highMz))
}
