package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.js.widget.Gui.DomElement
import Gui._
import org.scalajs.dom.html.Element

object Widgets {

    class Deck[K](widgets : Map[K, DomElement], initialShown : K) extends DomElement {
        private val elements = widgets.mapValues(_.toDom)
        private var shown = initialShown

        private def showElement(e : Element, tag : Boolean): Unit = tag match {
            case true => e.style.display = "block" // TODO
            case false => e.style.display = "none"
        }

        for((key, e) <- elements) showElement(e, key == shown)

        override def toDom: Element = {
            div()(elements.values.map(NodeElement).toSeq : _*).toDom
        }

        def show(key : K) : Unit = {
            showElement(elements(shown), tag = false)
            showElement(elements(key), tag = true)
        }
    }
}
