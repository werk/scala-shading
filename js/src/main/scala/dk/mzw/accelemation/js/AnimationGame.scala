package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.Animation
import org.scalajs.dom

object AnimationGame {

    def apply(animation : Animation, update : (Double, AnimationCanvas) => Unit, onClick : (Double, Double) => Unit) : Unit = {
        val canvas = new AnimationCanvas(animation)
        dom.document.getElementById("widget").appendChild(canvas.canvas)
        val start = System.currentTimeMillis()
        def step(elapsed : Double) : Unit = {
            val now = System.currentTimeMillis()
            val t = (now - start) / 1000.0
            update(t, canvas)
            canvas.update(t)
            dom.window.requestAnimationFrame(step _)
        }
        step(0)
    }

}
