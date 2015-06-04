package se.lth.immun

import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._

import scala.util.{Try, Success, Failure}

case class SourceDef(ip:String, port:Int, name:String)

class AddSourceDialog(
		onClose:Option[SourceDef] => Unit
) extends Dialog {
	
	val ipField = new TextField
	val portField = new TextField
	val nameField = new TextField
	val sourceStatus = new Label
	
	val cancel = Button("cancel") {
		onClose(None)
		close
	}
	
	val ok = Button("ok") {
		parseSource match {
			case Success(source) =>
				onClose(Some(source))
				close
			case Failure(e) =>
				sourceStatus.text = e.getMessage
		}
	}
	
	
	def parseSource:Try[SourceDef] = {
		Try(SourceDef(
			ipField.text,
			portField.text.toInt,
			nameField.text
		))
	}
	
	contents = new BorderPanel {
		val center = new GridPanel(3,2) {
			contents += new Label("ip")
			contents += ipField 
			contents += new Label("port")
			contents += portField
			contents += new Label("name")
			contents += nameField
		}
		val bottom = new GridPanel(2, 1) {
			contents += sourceStatus
			contents += new GridPanel(1, 2) {
				contents += cancel
				contents += ok
			}
		}
		layout(center) = Center
		layout(bottom) = South
	}
}