package dk.mzw.accelemation.js

import org.scalajs.dom

trait Widget {
    def onResize(width : Int, height : Int) : Unit
    def onDraw() : Unit
    val element : dom.Element
}
