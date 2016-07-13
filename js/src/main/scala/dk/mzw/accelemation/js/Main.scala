package dk.mzw.accelemation.js

import dk.mzw.accelemation.Animations._
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Internal.Uniform
import dk.mzw.accelemation.Language._

import scala.scalajs.js.JSApp

object Main extends JSApp {

    val width = 2
    val height = 2

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

    val angles = new Uniform[Array[Double]]("angles", Array.ofDim(width * height))

    val cells = angles.value.zipWithIndex.map{case (_, i) =>
        new RotatedCell(i)
    }


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

        cells.foreach(_.update(dt))
    }

    def cellCoordinates(x : Double, y : Double) : (Int, Int) = {
        val columnIndex = scala.math.floor((1 + x) * 0.5 * width).toInt
        val rowIndex = scala.math.floor((1 + y) * 0.5 * height).toInt
        (columnIndex, rowIndex)
    }

    def cellIndex(columnIndex : Int, rowIndex : Int) : Int = rowIndex * height + columnIndex

    def onClick(x : Double, y : Double) : Unit = {
        clickX.value = x
        clickY.value = y

        // Rotate clicked cell
        val (columnIndex, rowIndex) = cellCoordinates(x, y)
        val cell : RotatedCell = cells(cellIndex(columnIndex, rowIndex))
        cell.click()
    }

    def main() = AnimationGame(a, update, onClick)

    class RotatedCell(index : Int) {
        val rotationSpeed = 2
        def angle = angles.value(index)
        var angleFinal : Double = 0

        def click() = {
            angleFinal += scala.math.Pi * 0.5
            println(s"($index): angle: $angle")
            println(s"($index): angleFinal: $angleFinal")
        }

        def update(dt : Double): Unit = {
            angles.value(index) = scala.math.min(angleFinal, angle + dt * rotationSpeed)
        }

    }
}

