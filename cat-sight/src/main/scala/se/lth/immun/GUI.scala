package se.lth.immun

import scala.swing._
import scala.swing.BorderPanel.Position._
import java.awt.Color

class GUI(params: CatSightParams) extends SimpleSwingApplication {


	val textArea = new TextArea {
		text = "initial text\nline two"
		background = Color.green
	}
	val loadAssay = new Button { text = "load assay" }
	val gridPanel = new GridPanel(1, 2) {
		contents += loadAssay
		contents += new Label { text = "I'm a label" }
		contents += textArea
	}
	
	val plotBuffer = new PlotBuffer
	
	val top = new MainFrame {
		title = "Cat Sight MS data visualizer"
		preferredSize = new Dimension(1000, 700)
		contents = new BorderPanel {
			layout(gridPanel) = North
			layout(plotBuffer) = Center
			/*layout(button) = West
		      layout(canvas) = Center
		      layout(toggle) = East
		      layout(textField) = South
		      * 
		      */
		}
	}
	if (top.size == new Dimension(0, 0)) top.pack()
	top.visible = true
}