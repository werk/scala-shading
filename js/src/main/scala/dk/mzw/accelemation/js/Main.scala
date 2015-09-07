package dk.mzw.accelemation.js

import dk.mzw.accelemation.js.ViewState.Pick0
import dk.mzw.accelemation.samples._
import org.scalajs.dom
import org.scalajs.dom.raw.UIEvent

import scala.scalajs.js.JSApp

object Main extends JSApp {
    val animations = List(
        TimeLens.apply,
        HidingDevils.apply,
        Spiral.apply
    )

    def main(): Unit = {

        var activeWidget : Widget = null
        dom.window.onresize = { event : UIEvent =>
            activeWidget.onResize(dom.window.innerWidth, dom.window.innerHeight)
        }

        val widgetElement = dom.document.getElementById("widget")
        def setWidget(widget : Widget): Unit = {
            while(widgetElement.firstChild != null) widgetElement.removeChild(widgetElement.firstChild)
            activeWidget = widget
            widgetElement.appendChild(widget.element)
            dom.window.onresize(null)
        }
        def setViewState(viewState : ViewState) : Unit = {
            setWidget(ViewState.render(viewState, setViewState))
        }

        setWidget(new ListWidget(Pick0, setViewState))

        def step(elapsed : Double) : Unit = {
            activeWidget.onDraw()
            dom.window.requestAnimationFrame(step _)
        }
        step(0)
    }

}
