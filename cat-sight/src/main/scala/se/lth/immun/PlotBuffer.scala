package se.lth.immun

import scala.swing._
import scala.swing.event._
import java.awt.Color
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import se.jt.PlotsControl
import se.jt.PlotControl

import CatSightPrimaries._

object PlotBuffer {
	case class Marker[D, X, Y](ctrl:PlotControl[D, X, Y], px:Int, py:Int, mm:MouseMode)
	
	trait MouseMode
	case object XZoom extends MouseMode
	case object XYZoom extends MouseMode
	case object YZoom extends MouseMode
	case object Select extends MouseMode
	
	trait PlotBufferEvent extends Event { def id:PlotID }
	trait PlotBufferZoomEvent extends PlotBufferEvent
	case class NewZoom(id:PlotID, f:(TracePlotter.Datum, Int) => Boolean) extends PlotBufferZoomEvent
	case class PopZoom(id:PlotID, n:Int) extends PlotBufferZoomEvent
	case class Select(id:PlotID, f:(TracePlotter.Datum, Int) => Boolean) extends PlotBufferEvent
	case class NewSize(id:PlotID, size:Dimension) extends PlotBufferEvent
}

class PlotBuffer(val id:PlotID) extends Component {

	import TracePlotter._
	import PlotBuffer._
	
	var _img:Option[BufferedImage] = None
	var _control:Option[PlotsControl[Datum, Datum, Datum]] = None
	
	var m1:Option[Marker[Datum, Datum, Datum]] = None
	var m2:Option[Marker[Datum, Datum, Datum]] = None
	
	listenTo(this)
	reactions += {
		case e:UIElementResized => publish(NewSize(id, size))
		case fl:FocusLost =>
	}

	def setImg(img:BufferedImage) = {
		_img = Some(img)
		repaint
	}
	
	def setControl(ctrl:PlotsControl[Datum, Datum, Datum]) = {
		_control = Some(ctrl)
		m1 = None
		m2 = None
		repaint
	}
	
	def clear = {
		_img = None
		_control = None
		repaint
	}
	
	var mouseMode:MouseMode = XZoom
	def setMouseMode(mm:MouseMode) = {
		mouseMode = mm
		for (Marker(ctrl, px, py, _) <- m1)
			m1 = Some(Marker(ctrl, px, py, mm))
		repaint
	}
	
	listenTo(mouse.moves, mouse.clicks, keys, this)
	reactions += {
		case me:MouseEntered =>
			requestFocusInWindow
			
		case mm:MouseMoved =>
			for (control <- _control) {
				control.getControl(mm.point.x, mm.point.y) match {
					case Some(ctrl) =>
						m1 = Some(Marker(ctrl, mm.point.x, mm.point.y, mouseMode))
						repaint
						
					case None => 
						m1 = None
						repaint
				}
			}
			
		case md:MouseDragged =>
			for (control <- _control) {
				control.getControl(md.point.x, md.point.y) match {
					case Some(ctrl) =>
						m2 = Some(Marker(ctrl, md.point.x, md.point.y, mouseMode))
						repaint
						
					case None => {}
				}
			}
			
		case mr:MouseReleased => 
			if (mr.peer.getButton == java.awt.event.MouseEvent.BUTTON1) {
				for {
					Marker(ctrl, px1, py1, mm1) <- m1
					Marker(_, px2, py2, _) <- m2
				} {
					mm1 match {
						case XZoom 	=> publish(NewZoom(id, ctrl.zoomXFilter(px1, px2)))
						case YZoom 	=> publish(NewZoom(id, ctrl.zoomYFilter(py1, py2)))
						case XYZoom => publish(NewZoom(id, (d, i) => {
								val xOk = ctrl.zoomXFilter(px1, px2)(d, i) 
								val yOk = ctrl.zoomYFilter(py1, py2)(d, i)
								xOk && yOk
							}))
						case Select => publish(Select(id, ctrl.zoomXFilter(px1, px2)))
							
					}
				}
			} else 
				publish(PopZoom(id, 1))
			repaint
			
		case KeyPressed(_, Key.Z,_,_) => setMouseMode(XYZoom)
		case KeyPressed(_, Key.Y,_,_) => setMouseMode(YZoom)
		case KeyPressed(_, Key.S,_,_) => setMouseMode(Select)
		case KeyPressed(_,_,_,_) => println("only z (xy zoom), y (y zoom) and s (select peak) mouse modes are supported")
			
		case kr:KeyReleased => setMouseMode(XZoom)
	}
		
	
	override def paintComponent(g: Graphics2D) {
		
		def plotVertMarker[D, X, Y](m:Option[Marker[D, X, Y]]) =
			for (Marker(ctrl, px, py, mm) <- m) {
				val cx = ctrl.confineX(px)
				val cy0 = ctrl.confineY(0)
				val cy1 = ctrl.confineY(1000000)
				g.drawLine(cx, cy0, cx, cy1)
			}
		
		def plotHorMarker[D, X, Y](m:Option[Marker[D, X, Y]]) =
			for (Marker(ctrl, px, py, mm) <- m) {
				val cx0 = ctrl.confineX(-1000000)
				val cx1 = ctrl.confineX(1000000)
				val cy = ctrl.confineY(py)
				g.drawLine(cx0, cy, cx1, cy)
			}
		
		_img match {
			case Some(img) =>
				g.drawImage(img, null, 0, 0)
					
				for (Marker(ctrl, px1, py1, mm) <- m1) {
					mm match {
						case XZoom =>
							g.setColor(Color.BLACK)
							plotVertMarker(m1)
							plotVertMarker(m2)
							
						case YZoom =>
							g.setColor(Color.BLACK)
							plotHorMarker(m1)
							plotHorMarker(m2)
							
						case Select =>
							g.setColor(Color.RED)
							plotVertMarker(m1)
							plotVertMarker(m2)
							
						case XYZoom =>
							m2 match {
								case Some(Marker(_, px2, py2, _)) =>
									g.setColor(Color.BLACK)
									val x1 = ctrl.confineX(px1)
									val y1 = ctrl.confineY(py1)
									val x2 = ctrl.confineX(px2)
									val y2 = ctrl.confineY(py2)
									g.drawRect(math.min(x1, x2), math.min(y1, y2), math.abs(x1-x2), math.abs(y1-y2))
									
								case None =>
									g.setColor(Color.BLACK)
									val x1 = ctrl.confineX(px1)
									val y1 = ctrl.confineY(py1)
									g.drawLine(x1-10, y1, x1+10, y1)
									g.drawLine(x1, y1-10, x1, y1+10)
							}
							
					}
				}
				
			case None => 
				g.setColor(Color.BLACK)
				g.drawRect(1, 1, size.width - 2, size.height - 2)
				g.drawLine(1, 1, size.width - 2, size.height - 2)
				g.drawLine(1, size.height - 2, size.width - 2, 1)
		}
	}
}