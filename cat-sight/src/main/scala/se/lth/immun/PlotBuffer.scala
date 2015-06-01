package se.lth.immun

import scala.swing._
import java.awt.Graphics2D
import java.awt.image.BufferedImage

class PlotBuffer extends Panel {

	var _img:Option[BufferedImage] = None
	
	def setImg(img:BufferedImage) = {
		_img = Some(img)
		repaint
	}
	
	override def paintComponent(g: Graphics2D) {
		_img match {
			case Some(img) =>
				g.drawImage(img, null, 0, 0)
				
			case None => {}
		}
	}
}