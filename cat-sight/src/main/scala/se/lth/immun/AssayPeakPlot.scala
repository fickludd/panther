package se.lth.immun

import CatSightPrimaries._
import TracePlotter._

import java.awt.Graphics2D
import java.awt.Color
import java.awt.BasicStroke
import se.jt.{Plot, Geom, Scale, PixelSpace}
import Geom.Rect

import scala.collection.mutable.ArrayBuffer

class AssayPeakPlot 
	extends Plot[Datum, Datum, Datum, Seq[AssayTracePeak]] {

	type Self = AssayPeakPlot
	
	val peaks = new ArrayBuffer[AssayTracePeak]
	def remove(peakID:Int) = 
		peaks.remove(peaks.indexWhere(_.peakID != peakID))
	
	val dashed = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(8), 0)
	
	def checkAndSetup(data: Seq[Datum]) = (null, null, peaks)
	def data:Seq[Datum] = Nil
	def renderData(
			g:Graphics2D, 
			r:Rect, 
			xScale:Scale[Datum], 
			yScale:Scale[Datum],
			data:Seq[AssayTracePeak]
	) = {
		val ps = new PixelSpace(r)
		
		val regStroke = g.getStroke
		g.setColor(Color.BLACK)
		
		def toPx(rtStart:Double, rtApex:Double, rtEnd:Double) = {
			val xs = xScale(Array(Datum(rtStart, 0, ""), Datum(rtApex, 0, ""), Datum(rtEnd, 0, "")))
			(ps.toX(xs(0)), ps.toX(xs(1)), ps.toX(xs(2)))
		}
		
		def toPy(intMax:Double) = {
			val ys = yScale(Array(Datum(0, 0, ""), Datum(0, intMax, "")))
			(ps.toY(ys(0)), ps.toY(ys(1)))
		}
		
		for (AssayTracePeak(id, peakID, intSum, intMax, rtStart, rtApex, rtEnd) <- data) {
			val (pxStart, pxApex, pxEnd) = toPx(rtStart, rtApex, rtEnd)
			val (py0, pyMax) = toPy(intMax)
			
			g.setStroke(dashed)
			g.drawLine(pxStart, py0, pxStart, pyMax)
			g.drawLine(pxEnd, py0, pxEnd, pyMax)
			g.drawLine(pxStart, pyMax, pxEnd, pyMax)
			
			g.setStroke(regStroke)
			g.drawOval(pxApex-4, pyMax-4, 8, 8)
			if (math.abs(pxEnd - pxStart) > 50)
				g.drawString("%d, %.2e".format(peakID, intSum), pxStart, pyMax-4)
			else if (math.abs(pxEnd - pxStart) > 10)
				g.drawString("%d".format(peakID), pxStart, pyMax-4)
		}
		
	}
}