package se.lth.immun

import java.awt.KeyEventDispatcher
import java.awt.event.KeyEvent

import scala.collection.mutable.HashMap

object GlobalKeyBinder extends KeyEventDispatcher {
	
	val bindings = new HashMap[(Int, Int), () => Unit]
	
	def dispatchKeyEvent(e:KeyEvent):Boolean = 
		bindings.get((e.getKeyCode, e.getModifiers)) match {
			case Some(f) if e.getID == KeyEvent.KEY_PRESSED => 
				//println("Global key bound")
				f()
				true
			case _ => 
				//println("Global key unbound, code=%d, char=%s, mods=%d, id=%d".format(e.getKeyCode, e.getKeyChar.toString, e.getModifiers, e.getID))
				false
		}
}