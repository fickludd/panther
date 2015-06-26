package se.lth.immun

class IDGenerator {

	private var id:Int = -1
	def next = { id += 1; id }
}