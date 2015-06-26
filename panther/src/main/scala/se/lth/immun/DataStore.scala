package se.lth.immun

case class DataSpectrum(time: Float, mz: Seq[Double], intensity: Seq[Float]) {

	def sum(lowMz: Double, highMz: Double): Float = bigStepMethod(lowMz, highMz)

	def bigStepMethod(lowMz: Double, highMz: Double): Float = {
		val i0 = calcStepIndex(lowMz, 0)
		var in = 0
		if (highMz > mz(mz.size - 1)) {
			in = mz.size
		} else {
			in = calcStepIndex(highMz, i0)
		}
		var sum = 0.0f
		if (lowMz > mz(i0)) {
			return 0
		}

		for (i <- i0 until in) {
			sum += intensity(i)
		}

		return sum
	}

	def calcStepIndex(border: Double, index: Int): Int = {

		var iterations = 0
		var i = index
		while (true) {
			iterations += 1
			if (i > mz.size - 1) {
				i = mz.size - 1
			}

			if (mz(i) > border) {
				while (true) {

					iterations += 1
					i -= 100
					if (i < 0) {
						i = 0
					}
					if (mz(i) < border) {
						while (true) {

							iterations += 1
							i += 10
							if (i > mz.size - 1) {
								i = mz.size - 1
							}
							if (mz(i) > border) {
								while (true) {
									i -= 1
									if (mz(i) < border) {
										return i + 1

									}
								}
							}
						}
					} else if (i == 0) {
						return 0
					}

				}
			} else if (i == mz.size - 1) {
				return i
			} else {
				i += 1000

			}
		}

		return 0
	}

}

case class DataTrace(time: Seq[Float], intensity: Seq[Float])

object DataStore {
	trait Status
	case object UnInitialized extends Status
	case object Ready extends Status
	case class Loading(loaded: Int, total: Int) extends Status
}

trait DataStore {
	def extractL1Trace(
		lowMz: Double,
		highMz: Double): DataTrace

	def extractL2Trace(
		precLowMz: Double,
		precHighMz: Double,
		fragLowMz: Double,
		fragHighMz: Double): DataTrace

	def status: DataStore.Status
}

object DataStorer {
	case class MzRange(low: Double, high: Double)
	type PrecDef = Set[MzRange]
}

trait DataStorer {
	def setNumSpectra(n: Int): Unit
	def addL1Spectrum(ds: DataSpectrum): Unit
	def addL2Spectrum(key: DataStorer.PrecDef, ds: DataSpectrum): Unit
}

