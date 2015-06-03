package se.lth.immun

import scala.swing._
import scala.swing.BorderPanel.Position._
import java.awt.Color

class GUI(params: CatSightParams) extends SimpleSwingApplication {


	
	val addAssays = new Button { text = "add" }
	val clearAssays = new Button { text = "clear" }
	val searchField = new TextField
	val saveAssays = new Button { text = "save" }
	
	val plotBuffer = new PlotBuffer
	
	val assayList = new ListView[Assay]
	assayList.listData = (0 until 10).map(i => Assay.random(3, 6))
	
	val assayColumn = new BorderPanel {
		val topGrid = new GridPanel(1, 2) {
			contents += addAssays
			contents += clearAssays
		} 
		val bottomGrid = new GridPanel(2, 1) {
			contents += searchField
			contents += saveAssays
		}
		layout(topGrid) = North
		layout(assayList) = Center
		layout(bottomGrid) = South
	}
	val top = new MainFrame {
		title = "Cat Sight MS data visualizer"
		preferredSize = new Dimension(1000, 700)
		contents = new BorderPanel {
			//layout(gridPanel) = North
			layout(plotBuffer) = Center
			layout(assayColumn) = East
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