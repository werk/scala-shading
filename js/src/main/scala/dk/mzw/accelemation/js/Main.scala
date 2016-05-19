package dk.mzw.accelemation.js

import dk.mzw.accelemation.Animations._
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Internal.Uniform
import dk.mzw.accelemation.Language._
import org.scalajs.dom
import org.scalajs.dom.raw.MouseEvent

import scala.scalajs.js.JSApp

object Main extends JSApp {


    def main() : Unit = {

        val mouseX = new Uniform[Double]("mouseX", 0)
        val mouseY = new Uniform[Double]("mouseY", 0)
        val a : Animation = {
            addition (gaussBall(0.05)) (translate(mouseX, mouseY) (gaussBall(0.1)))
        }


        val (canvas, update) = AnimationCanvas(a)
        dom.document.getElementById("widget").appendChild(canvas)

        var cursorX : Double = 0
        var cursorY : Double = 0
        dom.document.onmousemove = {e : MouseEvent => {
            cursorX = e.pageX
            cursorY = e.pageY
        }}


        val start = System.currentTimeMillis()
        def step(elapsed : Double) : Unit = {
            val now = System.currentTimeMillis()
            val t = (now - start) / 1000.0
            val (x, y) = pixelToUnit(cursorX, cursorY)
            mouseX.value = x
            mouseY.value = y
            update(t)
            dom.window.requestAnimationFrame(step _)
        }
        step(0)
    }

    def pixelToUnit(pixelX : Double, pixelY : Double) : (Double, Double) = {
        val resolutionX = dom.window.innerWidth
        val resolutionY = dom.window.innerHeight
        val aspectX = scala.math.max(resolutionX / resolutionY, 1.0)
        val aspectY = scala.math.max(resolutionY / resolutionX, 1.0)
        val strechedPositionX = (pixelX / resolutionX) * 2.0 - aspectX
        val strechedPositionY = (pixelY / resolutionY) * 2.0 - aspectY
        (-strechedPositionX * aspectX, strechedPositionY * aspectY)

    }

}
