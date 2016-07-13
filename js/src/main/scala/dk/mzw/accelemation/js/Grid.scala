package dk.mzw.accelemation.js

import dk.mzw.accelemation.Internal.Uniform
import dk.mzw.accelemation.Language.Math._
import dk.mzw.accelemation.Language._

object Grid {

    type Transformer = ((R, R)) => (R, R)

    def apply(animation : Animation, angles : Uniform[Array[Double]]) : Animation = {
        applyTransformer(rotatedCells(angles)) (animation)
    }

    def applyTransformer(transformer: Transformer) (animation: Animation) : Animation = t => x => y => {
        val (x2, y2) = transformer((x, y))
        animation (t) (x2) (y2)
    }

    def rotatedCells(angles: Uniform[Array[Double]]) : Transformer = { case (x, y) =>
        val width = scala.math.sqrt(angles.value.length).toInt
        val height = width
        if(width * height != angles.value.length) throw new RuntimeException("Bad cell layout")
        val cellWidth = 2.0 / width
        val cellHeight = 2.0 / height
        val cellIndex = floor((1 + x) * 0.5 * width)
        val rowIndex = floor((1 + y) * 0.5 * height)
        val cellCenterX : R = cellIndex * cellWidth + (0.5 * cellWidth : R) - 1
        val cellCenterY : R = rowIndex * cellHeight + (0.5 * cellHeight : R) - 1
        val angles2 = liftUniformRArray(angles)
        val angle : R = angles2(rowIndex * width + cellIndex)
        rotateAbout(cellCenterX, cellCenterY, angle) ((x, y))
    }

    def rotateAbout(centerX : R, centerY : R, angle : R) : Transformer =
        translate(-centerX, -centerY)
            .andThen(rotate(angle))
            .andThen(translate(centerX, centerY))

    def rotate(angle : R) : Transformer = { case (x, y) =>
        (x * cos(angle) - y * sin(angle), x * sin(angle) + y * cos(angle))
    }

    def translate(dx : R, dy : R) : Transformer = { case (x, y) => (x + dx, y + dy)}



}
