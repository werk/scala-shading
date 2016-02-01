package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Combinators._

object Tiled {

    def void : Animation = {t => x => y => rgba(0.5, 0.5, 0.5, 1)}

    def grid(animations : List[Animation]) : Animation = { t => x => y =>
        val lines = scala.math.floor(scala.math.sqrt(animations.size)).toInt
        val animationLines = animations.grouped(lines).map(a => horizontal(void :: a ++ List(void))).toList
        val animationGrid = vertical(void :: animationLines ++ List(void))
        val corrected = scale (0.1, 0.1) (translate (lines / -2, -1) (animationGrid)) _
        corrected (t) (x) (y)
    }

    def vertical(animations : List[Animation]) : Animation = {t => x => y =>
        def makeIfs(as : List[(Animation, Int)]) : Color = as match {
            case List((a, i)) => a (t) (x) (i + y)
            case List((a1, i1), (a2, i2)) => if_ (y > (-i1*2 - 1), a1 (t) (x) (i1*2 + y), a2 (t) (x) (i2*2 + y))
            case more =>
                val half = more.length / 2
                val left = more.take(half)
                val (_, i) = left.last
                val right = more.drop(half)
                if_ (y > (-i*2 - 1), makeIfs(left), makeIfs(right))
        }
        makeIfs(animations.zipWithIndex)
    }

    def horizontal(animations : List[Animation]) : Animation = {t => x => y =>
        def makeIfs(as : List[(Animation, Int)]) : Color = as match {
            case List((a, i)) => a (t) (i + x) (y)
            case List((a1, i1), (a2, i2)) => if_ (x > (-i1*2 - 1), a1 (t) (i1*2 + x) (y), a2 (t) (i2*2 + x) (y))
            case more =>
                val half = more.length / 2
                val left = more.take(half)
                val (_, i) = left.last
                val right = more.drop(half)
                if_ (x > (-i*2 - 1), makeIfs(left), makeIfs(right))
        }
        makeIfs(animations.zipWithIndex)
    }

}
