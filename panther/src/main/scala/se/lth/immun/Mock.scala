package se.lth.immun

import scala.util.Random

object Mock {
	
	def small(ds:SimpleDataStore) = {
		def mockSpectrum(t:Double, int1:Double, int2:Double):DataSpectrum = 
			DataSpectrum(t, 
				400.0 until 600.0 by 0.1, 
				Array(int1).padTo(1000, int1).padTo(2000, int2)
			)
			
		ds.addL1Spectrum(mockSpectrum(1.0, 1.0, 2.0))
		ds.addL1Spectrum(mockSpectrum(2.0, 3.0, 4.0))
		ds.addL1Spectrum(mockSpectrum(3.0, 5.0, 6.0))
		ds.addL2Spectrum(512.5, mockSpectrum(10.0, 10.0, 20.0))
		ds.addL2Spectrum(512.5, mockSpectrum(20.0, 30.0, 40.0))
		ds.addL2Spectrum(512.5, mockSpectrum(30.0, 50.0, 60.0))
		ds.addL2Spectrum(537.5, mockSpectrum(100.0, 100.0, 200.0))
		ds.addL2Spectrum(537.5, mockSpectrum(200.0, 300.0, 400.0))
		ds.addL2Spectrum(537.5, mockSpectrum(300.0, 500.0, 600.0))
	}
	
	
	def big(ds:SimpleDataStore) = {
		def mockSpectrum(t:Double):DataSpectrum = {
			val mzs = 400.0 until 1200.0 by 1 
			DataSpectrum(t, 
				mzs, 
				mzs.map(d => 1.0)
			)
		}
		
		val t0 = System.currentTimeMillis
		val nSpectraPerMatrix = 78000 / 33
		for (i <- 0 until nSpectraPerMatrix) {
			ds.addL1Spectrum(mockSpectrum(i))
			for (j <- 0 until 32)
				ds.addL2Spectrum(412.5 + j*25, mockSpectrum(i))
				
			if (i % 200 == 0) 
				println("at spectrum %8d %8ds   %d / %d Mb".format(
					i,
					(System.currentTimeMillis - t0) / 1000, 
					Runtime.getRuntime().totalMemory() / 1000000, 
					Runtime.getRuntime().maxMemory() / 1000000))
		}
			
	}
	
}