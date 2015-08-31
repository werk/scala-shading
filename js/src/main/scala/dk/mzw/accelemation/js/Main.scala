package dk.mzw.accelemation.js

import dk.mzw.accelemation.ToGlsl
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
        val canvas = dom.document.getElementById("canvas")
        val animade = new Animade(Animade.Configuration(ToGlsl(animations.head), canvas))
        dom.window.onresize = { event : UIEvent =>
            animade.resize(dom.window.innerWidth, dom.window.innerHeight)
        }
        dom.window.onresize(null)
        val start = System.currentTimeMillis()
        def step(elapsed : Double) : Unit = {
            val now = System.currentTimeMillis()
            animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
            dom.window.requestAnimationFrame(step _)
        }
        step(0)
    }

}
