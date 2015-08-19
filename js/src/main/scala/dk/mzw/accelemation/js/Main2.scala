package dk.mzw.accelemation.js

import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.samples._
import org.scalajs.dom.{window, document}
import org.scalajs.dom.raw.{UIEvent, MouseEvent}
import scala.scalajs.js.JSApp
import scala.scalajs.js


object Main2 extends JSApp {
    val animations = List(
        TimeLens.apply,
        HidingDevils.apply,
        Spiral.apply
    )

    def main(): Unit = {

        var shaded : Shaded = null
        val startTime = new js.Date()

        def load(code : String): Unit = {
            if(shaded != null) {
                document.body.removeChild(shaded.canvas)
                shaded.dispose()
            }

            shaded = new Shaded(code, startTime)
            document.body.appendChild(shaded.canvas)

            def onResize(event : UIEvent) : Unit = shaded.resize(window.innerWidth, window.innerHeight)
            window.onresize = onResize _
            onResize(null)

            def step(something : Double) {
                shaded.step()
                window.requestAnimationFrame(step _)
            }
            step(0)
        }

        var i = 0
        load(ToGlsl(animations(i)))

        document.body.onmousedown = {e : MouseEvent =>
            i = (i + 1) % animations.length
            val animation = animations(i)
            val code = ToGlsl(animation)
            load(code)
        }
    }

    //case class DefinedAnimation(animation : Animation, )


}
