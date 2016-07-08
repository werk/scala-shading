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

    val clickX = new Uniform[Double]("clickX", 0)
    val clickY = new Uniform[Double]("clickY", 0)


    val rotation = new Uniform[Double]("rotation", 0)
    var rotationStarted : Double = 0
    var rotationFinal : Double = 0

    val a : Animation = {
        val grid = rotate (rotation) (HidingDevils.apply) _
        val cursor = translate(mouseX, mouseY) (colorBall(0.05, rgba(1, 1, 1, 1)))
        val click = translate(clickX, clickY) (colorBall(0.05, rgba(1, 0.3, 0.2, 1)))
        addition (addition (grid) (cursor)) (click)
    }

    var lastTime : Double = 0
    def update(time : Double, canvas : AnimationCanvas): Unit = {
        lastTime = time
        val (x, y) = canvas.mouse
        mouseX.value = x
        mouseY.value = y

        // Rotate
        {
            val t = time - rotationStarted
            rotation.value = scala.math.min(rotationFinal, t * 3)
        }
    }

    def onClick(x : Double, y : Double) : Unit = {
        clickX.value = x
        clickY.value = y
        rotationStarted = lastTime
        rotationFinal += scala.math.Pi * 0.5
    }

    def main() = AnimationGame(a, update, onClick)
}
