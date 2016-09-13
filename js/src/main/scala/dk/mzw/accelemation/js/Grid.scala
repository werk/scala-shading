package dk.mzw.accelemation.js

import dk.mzw.accelemation.Internal.Uniform
import dk.mzw.accelemation.Language.Math._
import dk.mzw.accelemation.Language._

object Grid {

    type Transformer = ((R, R)) => (R, R)

    def apply(animcation : Animation, angleRows : List[List[Uniform[Double]]]) : Animation = {
        applyTransformer(rotatedCells(angleRows)) (animcation)
    }

    def applyTransformer(transformer: Transformer) (animation: Animation) : Animation = t => x => y => {
        val (x2, y2) = transformer((x, y))
        animation (t) (x2) (y2)
    }

    def rotatedCells(angleRows: List[List[Uniform[Double]]]) : Transformer = { case (x, y) =>
        val width = angleRows.head.length
        val height = angleRows.length
        val cellWidth = 2.0 / width
        val cellHeight = 2.0 / height
        val cellIndex = floor((1 + x) * 0.5 * width)
        val rowIndex = floor((1 + y) * 0.5 * height)
        val cellCenterX : R = 0
        val cellCenterY : R = 0
        val angle : R = horizontal(angleRows.head, x, y)
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


    def horizontal(angles : List[Uniform[Double]], x : R, y : R) : R = {
        def makeIfs(as : List[(Uniform[Double], Int)]) : R = as match {
            case List((a, m)) => a
            case List((a1, m1), (a2, m2)) => if_(x < (m1 + 1), a1 , a2)
            case more =>
                val half = more.length / 2
                val left = more.take(half)
                val (_, m) = left.last
                val right = more.drop(half)
                if_(x < (m + 1), makeIfs(left), makeIfs(right))
        }
        makeIfs(zipWithMidpoints(angles))
    }

    def midpoints(list : List[_]) : List[Int] = {
        zipWithMidpoints(list).unzip._2
    }

    def zipWithMidpoints[A](list : List[A]) : List[(A, Int)] = {
        val start = -scala.math.round(list.length.toDouble).toInt + 1
        val midpoints = Iterator.iterate(start) (_ + 2).take(list.size).toList
        list.zip(midpoints)
    }

}
