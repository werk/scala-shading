package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language._

object Scroller {

    def tiled(animations : List[Animation]) : Animation = {t => x => y =>
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
}
