package dk.mzw.accelemation.js

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
        val activeWidget = new AnimationWidget(animations.head)

        dom.window.onresize = { event : UIEvent =>
            activeWidget.onResize(dom.window.innerWidth, dom.window.innerHeight)
        }
        dom.window.onresize(null)

        def step(elapsed : Double) : Unit = {
            activeWidget.onDraw()
            dom.window.requestAnimationFrame(step _)
        }
        step(0)
    }

}
