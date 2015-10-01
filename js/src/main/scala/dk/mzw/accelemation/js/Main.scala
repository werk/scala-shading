package dk.mzw.accelemation.js

import dk.mzw.accelemation.js.BuildOrder.{Effect, Id}
import dk.mzw.accelemation.js.ViewState.{ShowList, Pick0}
import dk.mzw.accelemation.js.widget.Widget
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

    def main() : Unit = {

        var activeWidget : Widget = null
        dom.window.onresize = { event : UIEvent =>
            activeWidget.onResize(dom.window.innerWidth, dom.window.innerHeight)
        }

        val buildAnimation = Prelude.buildAnimation

        val widgetElement = dom.document.getElementById("widget")
        def setWidget(widget : Widget): Unit = {
            while(widgetElement.firstChild != null) widgetElement.removeChild(widgetElement.firstChild)
            activeWidget = widget
            widgetElement.appendChild(widget.element)
            dom.window.onresize(null)
        }
        def reloadAnimations(onSuccess : () => Unit) : Unit = {
            def onList(animations : Seq[(Id, BuildOrder)]) : Unit = {
                for((id, build) <- animations) {
                    buildAnimation.animationMap += (id -> buildAnimation(build))
                }
                onSuccess()
            }
            LocalStore.store.list(onSuccess = onList, onError = println)
        }
        def setViewState(viewState : ViewState) : Unit = {
            def run() = setWidget(ViewState.render(viewState, setViewState, buildAnimation))
            if(viewState == ShowList(Pick0, 0)) reloadAnimations(run)
            else run()
        }

        reloadAnimations(() => {
            setViewState(ShowList(Pick0, 0))
        })

        def step(elapsed : Double) : Unit = {
            if(activeWidget != null) activeWidget.onDraw()
            dom.window.requestAnimationFrame(step _)
        }
        step(0)
    }

}
