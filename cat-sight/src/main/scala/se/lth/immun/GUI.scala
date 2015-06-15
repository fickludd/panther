package se.lth.immun

import scala.swing._
import scala.swing.BorderPanel.Position._
import java.awt.Color
import java.net.InetSocketAddress

import scala.collection.mutable.HashMap
import CatSightPrimaries._

class GUI(params: CatSightParams) extends SimpleSwingApplication {

	class RLabel(str:String) extends Label(str) {
		horizontalTextPosition = Alignment.Right
		horizontalAlignment = Alignment.Right
	}
	
	class SourceLabel(str:String) extends Label(str) {
		background = Color.BLACK
		foreground = Color.CYAN
		opaque = true
	}

	// ASSAY CONTROL COLUMN
	val assayList = new ListView[Assay]
	val addAssays = new Button { text = "add" }
	val clearAssays = new Button { text = "clear" }
	val filterStatus = new Label
	val posFilterField = new FilterField
	val negFilterField = new FilterField
	val saveAssays = new Button { text = "save" }
	val saveAll = new CheckBox { text = "save all" }
	
	// TRACE DISPLAY
	val sourceLegend = new GridPanel(1, 1)
	val plots = new GridPanel(1,1)
	val syncZoom = new CheckBox { text = "synz zoom" }
	val hideLegends = new CheckBox { text = "hide legends" }
	val addSource = new Button { text = "add source" }
	
	//plots.contents += new PlotBuffer
	
	//assayList.listData = (0 until 10).map(i => Assay.random(3, 6))
	
	val assayColumn = new BorderPanel {
		val topGrid = new GridPanel(1, 2) {
			contents += addAssays
			contents += clearAssays
		} 
		val bottomGrid = new GridPanel(4, 2) {
			contents += new Label
			contents += filterStatus
			contents += new RLabel("contains")
			contents += posFilterField
			contents += new RLabel("doesn't contain")
			contents += negFilterField
			contents += saveAll
			contents += saveAssays
		}
		layout(topGrid) = North
		layout(new ScrollPane(assayList)) = Center
		layout(bottomGrid) = South
		
		preferredSize = new Dimension(300, 2000)
	}
	val top = new MainFrame {
		title = "Cat Sight MS data visualizer"
		preferredSize = new Dimension(1000, 700)
		val traceControl = new GridPanel(1,2) {
			contents += syncZoom
			contents += hideLegends
			contents += addSource
		}
		val traceDisplay = new BorderPanel {
			layout(sourceLegend) = North
			layout(plots) = Center
			layout(traceControl) = South
		}
		
		contents = new BorderPanel {
			//layout(gridPanel) = North
			layout(traceDisplay) = Center
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
	
	def setPlots(traces:HashMap[PlotID, AssayTrace]) = {
		
		val sources = traces.keys.map(_.source).toSeq.sortBy(_.toString).distinct
		val assayIds = traces.keys.map(_.assayId).toSeq.sorted.distinct
		
		plots.columns = sources.length
		plots.rows = assayIds.length
		plots.contents.clear
		
		for {
			aid <- assayIds
			source <- sources
		} {
			traces.get(PlotID(aid, source)) match {
				case Some(trace) =>
					plots.contents += trace.plotBuffer
				case None =>
					plots.contents += new Label
			}
		}
		plots.revalidate
	}
	
	def setSources(sources:HashMap[InetSocketAddress, Source]) = {
		sourceLegend.columns = sources.size
		sourceLegend.contents.clear
		sourceLegend.contents ++= 
			sources.toSeq.sortBy(_._1.toString).map(t => new SourceLabel(t._2.name))
		sourceLegend.revalidate
	}
}