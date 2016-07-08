package dk.mzw.accelemation.js

import dk.mzw.accelemation.Animations._
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Internal.Uniform
import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.{HidingDevils, TimeLens}
import scala.scalajs.js.JSApp

object Main extends JSApp {

    def const(color : Color) : Animation = {t => x => y => color}
    def colorBall(variance : R, color : Color) : Animation = multiply (gaussBall(variance)) (const(color))

    val mouseX = new Uniform[Double]("mouseX", 0)
    val mouseY = new Uniform[Double]("mouseY", 0)


    val rotation = new Uniform[Double]("rotation", 0)
    val rotationStarted : Double = 0
    val rotationFinal : Double = 3

    val a : Animation = {
        val grid = rotate (rotation) (HidingDevils.apply) _
        val cursor = translate(mouseX, mouseY) (colorBall(0.05, rgba(1, 1, 1, 1)))
        addition (grid) (cursor)
    }

    def update(time : Double, canvas : AnimationCanvas): Unit = {
        val (x, y) = canvas.mouse
        mouseX.value = x
        mouseY.value = y

        // Rotate
        {
            val t = time - rotationStarted
            rotation.value = scala.math.min(rotationFinal, t * 0.5)
        }
    }

    def main() = AnimationGame(a, update)
}
