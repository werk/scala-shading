package dk.mzw.accelemation.js

import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.samples._
import org.scalajs.dom.raw.MouseEvent

import scala.scalajs.js.JSApp
import org.scalajs.dom

object Main extends JSApp {
    val animations = List(
        TimeLens.apply,
        HidingDevils.apply,
        Spiral.apply
    )

    def main(): Unit = {
        val glsl = new GlslContainer(dom.document.body)
        var i = 0
        glsl.load(ToGlsl(animations(i)))

        dom.document.body.onmousedown = {e : MouseEvent =>
            i = (i + 1) % animations.length
            val animation = animations(i)
            val code = ToGlsl(animation)
            glsl.load(code)
        }
    }

    //case class DefinedAnimation(animation : Animation, )


}
