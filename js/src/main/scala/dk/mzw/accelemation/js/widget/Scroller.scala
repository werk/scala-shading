package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Animations._
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Language.Math._
import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.{Spiral, HidingDevils, TimeLens}

object Scroller {

    def binary(animation: Time => R => R => B): Animation = { t => x => y =>
        if_(animation(t)(x)(y), 1, 0) bind {intensity =>
            rgba(intensity, intensity, intensity, 1)
        }
    }

    val circle: Animation = binary { t => x => y =>
        sqrt(x * x + y * y).bind{ distance =>
            distance > 0.5 && distance < 1
        }
    }

    val a1 = TimeLens.apply
    val a2 = HidingDevils.apply
    val a3 = Spiral.apply

    def tiled(animations : List[Animation]) : Animation = {t => x => y =>
        def makeIfs(as : List[(Animation, Int)]) : Color = as match {
            case List((a, i)) => a (t) (x) (i + y)
            case List((a1, i1), (a2, i2)) => if_ (y > (-i1*2 - 1), a1 (t) (x) (i1 + y), a2 (t) (x) (i2 + y))
            case more =>
                val half = more.length / 2
                val left = more.take(half)
                val (_, i) = left.last
                val right = more.drop(half)
                if_ (y > (-i*2 - 1), makeIfs(left), makeIfs(right))
        }

        def makeIfStrings(as : List[(String, Int)]) : String = as match {
            case List((s, i)) => s"$s (t) (x) ($i + y)"
            case List((s1, i1), (s2, i2)) => s"if_ (y > (${-i1*2 - 1}), $s1 (t) (x) ($i1 + y), $s2 (t) (x) ($i1 + y))"
            case more =>
                val half = more.length / 2
                val left = more.take(half)
                val (_, i) = left.last
                val right = more.drop(half)
                s"if_ (y > (${-i*2 - 1}), ${makeIfStrings(left)}, ${makeIfStrings(right)})"
        }

        if_ (y > -1, circle (t) (x) (y),
            if_ (y > -3, a2 (t) (x) (2 + y), a3 (t) (x) (4 + y))
        )

        println(makeIfStrings(List("a1", "a2", "a3").zipWithIndex))
        makeIfs(animations.zipWithIndex)
    }

    val scaled : Animation = translate(0, -1) (scale (0.5, 0.5) (tiled(List(circle, a1, a2, a3))))
}
