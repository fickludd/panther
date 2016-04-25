package se.lth.immun

import scala.util.Random

object Mock {
	
	import DataStorer._
	
	def small(ds:DataStorer) = {
		def mockSpectrum(t:Float, int1:Float, int2:Float):DataSpectrum = 
			DataSpectrum(t, 
				400.0 until 600.0 by 0.1, 
				Array(int1).padTo(1000, int1).padTo(2000, int2)
			)
			
		val swath1 = Set(MzRange(500.0, 525.0))
		val swath2 = Set(MzRange(525.0, 550.0))
			
		ds.addL1Spectrum(mockSpectrum(1.0f, 1.0f, 2.0f))
		ds.addL1Spectrum(mockSpectrum(2.0f, 3.0f, 4.0f))
		ds.addL1Spectrum(mockSpectrum(3.0f, 5.0f, 6.0f))
		ds.addL2Spectrum(swath1, mockSpectrum(10.0f, 10.0f, 20.0f))
		ds.addL2Spectrum(swath1, mockSpectrum(20.0f, 30.0f, 40.0f))
		ds.addL2Spectrum(swath1, mockSpectrum(30.0f, 50.0f, 60.0f))
		ds.addL2Spectrum(swath2, mockSpectrum(100.0f, 100.0f, 200.0f))
		ds.addL2Spectrum(swath2, mockSpectrum(200.0f, 300.0f, 400.0f))
		ds.addL2Spectrum(swath2, mockSpectrum(300.0f, 500.0f, 600.0f))
	}
	
	
	def big(ds:DataStorer) = {
		def mockSpectrum(t:Float):DataSpectrum = {
			val mzs = 400.0 until 1200.0 by 1 
			DataSpectrum(t, 
				mzs, 
				mzs.map(d => 1.0f)
			)
		}
		
		val swaths = (0 until 32).map(j => Set(MzRange(400.0+j*25, 425.0+j*25)))
		
		val t0 = System.currentTimeMillis
		val nSpectraPerMatrix = 78000 / 33
		for (i <- 0 until nSpectraPerMatrix) {
			ds.addL1Spectrum(mockSpectrum(i))
			for (j <- 0 until 32)
				ds.addL2Spectrum(swaths(j), mockSpectrum(i))
				
			if (i % 200 == 0) 
				println("at spectrum %8d %8ds   %d / %d Mb".format(
					i,
					(System.currentTimeMillis - t0) / 1000, 
					Runtime.getRuntime().totalMemory() / 1000000, 
					Runtime.getRuntime().maxMemory() / 1000000))
		}
			
	}
	
}