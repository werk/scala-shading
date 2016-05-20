package dk.mzw.accelemation.js

import dk.mzw.accelemation.Animations._
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Internal.Uniform
import dk.mzw.accelemation.Language._
import scala.scalajs.js.JSApp

object Main extends JSApp {

    def const(color : Color) : Animation = {t => x => y => color}
    def colorBall(variance : R, color : Color) : Animation = multiply (gaussBall(variance)) (const(color))

    val mouseX = new Uniform[Double]("mouseX", 0)
    val mouseY = new Uniform[Double]("mouseY", 0)
    val a : Animation = {
        addition (colorBall(0.05, rgba(0.8, 0.4, 0.6, 1))) (translate(mouseX, mouseY) (colorBall(0.05, rgba(0.3, 0.6, 0.5, 1))))
    }

    def update(time : Double, canvas : AnimationCanvas): Unit = {
        val (x, y) = canvas.mouse
        mouseX.value = x
        mouseY.value = y
    }

    def main() = AnimationGame(a, update)
}
