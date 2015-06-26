package se.lth.immun


import CatSightPrimaries._
import PlotBuffer._
import scala.collection.mutable.Map
	
object PlotActionBehaviour {
	
	val list = Array(Self, Source, Assay, All)
	
	case object Self extends PlotActionBehaviour {
		def send(
				e:PlotBufferEvent,
				assayTraces:Map[PlotID, AssayTrace]
		):Unit = 
			for (AssayTrace(plotter, buff) <- assayTraces.get(e.id))
				plotter ! e
	}
	
	case object Source extends PlotActionBehaviour {
		def send( 
				e:PlotBufferEvent,
				assayTraces:Map[PlotID, AssayTrace]
		):Unit = 
			for ((id, at) <- assayTraces.filter(_._1.source == e.id.source))
				at.plotter ! e
	}
	
	case object Assay extends PlotActionBehaviour {
		def send( 
				e:PlotBufferEvent,
				assayTraces:Map[PlotID, AssayTrace]
		):Unit = 
			for ((id, at) <- assayTraces.filter(_._1.assayId == e.id.assayId))
				at.plotter ! e
	}
	
	case object All extends PlotActionBehaviour {
		def send( 
				e:PlotBufferEvent,
				assayTraces:Map[PlotID, AssayTrace]
		):Unit = 
			for (AssayTrace(plotter, buff) <- assayTraces.values)
				plotter ! e
	}
}

trait PlotActionBehaviour {
	def send(e:PlotBufferEvent, assayTraces:Map[PlotID, AssayTrace]):Unit
}