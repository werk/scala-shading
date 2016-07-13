package dk.mzw.accelemation.js

import dk.mzw.accelemation.Animations._
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Internal.Uniform
import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.HidingDevils

import scala.scalajs.js.JSApp

object Main extends JSApp {

    val testAnimation : Animation = t => x => y => {
        vec2(x, y).magnitude.bind { d =>
            if_(d < 1,
                rgba(if_(x > 0, 1, 0), if_(y > 0, 1, 0), d, 1),
                scale(0.07, 0.04) (chess) (t) (x) (y)
            )
        }
    }

    def const(color : Color) : Animation = {t => x => y => color}
    def colorBall(variance : R, color : Color) : Animation = multiply (gaussBall(variance)) (const(color))

    val mouseX = new Uniform[Double]("mouseX", 0)
    val mouseY = new Uniform[Double]("mouseY", 0)

    val clickX = new Uniform[Double]("clickX", 0)
    val clickY = new Uniform[Double]("clickY", 0)

    val angles = new Uniform[Array[Double]]("angles", Array(0, 0, 0, 0))


    val a : Animation = {
        val grid = Grid(testAnimation, angles)
        val cursor = translate(mouseX, mouseY) (colorBall(0.05, rgba(1, 1, 1, 1)))
        val click = translate(clickX, clickY) (colorBall(0.05, rgba(1, 0.3, 0.2, 1)))
        addition (addition (grid) (cursor)) (click)
    }

    var lastTime : Double = 0
    def update(time : Double, canvas : AnimationCanvas): Unit = {
        val dt = time - lastTime
        lastTime = time
        val (x, y) = canvas.mouse
        mouseX.value = x
        mouseY.value = y

        cells.foreach(_.foreach(_.update(dt)))
    }

    def cellCoordinates(x : Double, y : Double) : (Int, Int) = {
        val width = cells.head.length
        val height = cells.length
        val cellIndex = scala.math.floor((1 + x) * 0.5 * width).toInt
        val rowIndex = scala.math.floor((1 + y) * 0.5 * height).toInt
        (cellIndex, rowIndex)
    }

    def onClick(x : Double, y : Double) : Unit = {
        clickX.value = x
        clickY.value = y

        // Rotate clicked cell
        val (cellIndex, rowIndex) = cellCoordinates(x, y)
        val cell = cells(rowIndex)(cellIndex)
        cell.click()
    }

    def main() = AnimationGame(a, update, onClick)
}

class RotatedCell(name: String) {
    val rotationSpeed = 2

    val rotation = new Uniform[Double](s"rotation_$name", 0)
    var rotationFinal : Double = 0

    def click() = {
        rotationFinal += scala.math.Pi * 0.5
        println(s"$name: rotation.value: ${rotation.value}")
        println(s"$name: rotationFinal: $rotationFinal")

    }

    def update(dt : Double): Unit = {
        rotation.value = scala.math.min(rotationFinal, rotation.value + dt * rotationSpeed)
    }

}