package se.lth.immun

import se.jt.Params

class LoadAssayParams extends Params {
	import Params._
	
	val splitPrecAndFrag = false	## "Split precursor and fragment traces into separate assays"
	val fragFilterRE = ""			## "Regex: fragments with matching traceIDs will be ignored"
	val fragSubGroupRE = ""			## "Regex defining how to split fragments into assays based on a label from the traceID"
	val precSubGroupRE = ""			## "Regex defining how to split precursors into assays based on a label from the traceID"
}