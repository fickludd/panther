package se.lth.immun

import scala.swing._
import scala.swing.event._
import java.awt.Color
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import se.jt.PlotsControl
import se.jt.PlotControl

object PlotBuffer {
	case class Marker[D, X, Y](ctrl:PlotControl[D, X, Y], px:Int, py:Int)
}

class PlotBuffer extends Component {

	import TracePlotter._
	import PlotBuffer._
	
	var _img:Option[BufferedImage] = None
	var _control:Option[PlotsControl[Datum, Datum, Datum]] = None
	
	var m1:Option[Marker[Datum, Datum, Datum]] = None
	var m2:Option[Marker[Datum, Datum, Datum]] = None
		
	var onNewZoom:((TracePlotter.Datum, Int) => Boolean) => Unit = f => {}
	var onZoomPop:Int => Unit = i => {}
	
	def setImg(img:BufferedImage) = {
		_img = Some(img)
		repaint
	}
	
	def setControl(ctrl:PlotsControl[Any, Any, Any]) = {
		_control = Some(ctrl.asInstanceOf[PlotsControl[Datum, Datum, Datum]])
		m1 = None
		m2 = None
		repaint
	}
	
	listenTo(this.mouse.moves)
	listenTo(this.mouse.clicks)
	reactions += {
		case mm:MouseMoved =>
			for (control <- _control) {
				control.getControl(mm.point.x, mm.point.y) match {
					case Some(ctrl) =>
						m1 = Some(Marker(ctrl, mm.point.x, mm.point.y))
						repaint
						
					case None => {}
				}
			}
			
		case md:MouseDragged =>
			for (control <- _control) {
				control.getControl(md.point.x, md.point.y) match {
					case Some(ctrl) =>
						m2 = Some(Marker(ctrl, md.point.x, md.point.y))
						repaint
						
					case None => {}
				}
			}
			
		case mr:MouseReleased => 
			if (mr.peer.getButton == java.awt.event.MouseEvent.BUTTON1) {
				for {
					Marker(ctrl, px1, _) <- m1
					Marker(_, px2, _) <- m2
				} {
					onNewZoom(ctrl.zoomXFilter(px1, px2))
				}
			} else 
				onZoomPop(1)
			repaint
	}
		
	
	override def paintComponent(g: Graphics2D) {
		_img match {
			case Some(img) =>
				g.drawImage(img, null, 0, 0)
					
				g.setColor(Color.BLACK)
				def plotMarker[D, X, Y](m:Option[Marker[D, X, Y]]) =
					for (Marker(ctrl, px, py) <- m) {
						val cx = ctrl.confineX(px)
						val cy0 = ctrl.confineY(0)
						val cy1 = ctrl.confineY(1000000)
						g.drawLine(cx, cy0, cx, cy1)
					}
				plotMarker(m1)
				plotMarker(m2)
				
			case None => {}
		}
	}
}