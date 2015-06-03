package se.lth.immun

import se.lth.immun.protocol.MSDataProtocol
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object Assay {
	case class Prec(id:String, mz:Double)
	case class Frag(id:String, precMz:Double, fragMz:Double)
	
	var randCount = 0
	def random(nPrec:Int, nFrag:Int) = {
		def randMz = 400.0 + scala.util.Random.nextDouble*800.0
		randCount += 1
		new Assay(
				"randomAssay"+randCount,
				for (i <- 0 until nPrec) yield Prec("prec %d-%d".format(randCount, i), randMz),
				for (i <- 0 until nFrag) yield Frag("frag %d-%d".format(randCount, i), randMz, randMz)
				)
	}
	
	class AssayBuilder {
		val precs = new ArrayBuffer[Prec]
		val frags = new ArrayBuffer[Frag]
	}
}

class Assay(
		val id:String,
		val precs:Seq[Assay.Prec],
		val frags:Seq[Assay.Frag]
		
) {
	
	override def toString = id
	
	def toTraceMsg(implicit params:CatSightParams) = {
		import MSDataProtocol._
		
		val req = GetTracesFor.newBuilder
		for (p <- precs) {
			val diff = p.mz * params.tracePPM / 1e6
			req.addPrecursor(Bounds.newBuilder.setLmz(p.mz-diff).setHmz(p.mz+diff))
		}
		for (Assay.Frag(id, pmz, fmz) <- frags) {
			val pDiff = pmz * params.tracePPM / 1e6
			val fDiff = fmz * params.tracePPM / 1e6
			req.addFragment(
				FragmentBounds.newBuilder
					.setPrecursor(Bounds.newBuilder.setLmz(pmz-pDiff).setHmz(pmz+pDiff))
					.setFragment(Bounds.newBuilder.setLmz(fmz-fDiff).setHmz(fmz+fDiff))
				)
		}
		MasterRequest.newBuilder.setGetTracesFor(req).build
	}
}