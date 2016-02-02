package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Combinators._

object Tiled {

    def void : Animation = {t => x => y => rgba(0.2, 0.2, 0.2, 1)}

    def grid(animations : List[Animation]) : (List[(Int, Int)], Animation) = {
        val lines = scala.math.floor(scala.math.sqrt(animations.size)).toInt
        val foo = animations.grouped(lines).toList
        val animationLines = foo.map(a => horizontal(void :: a ++ List(void)))
        val animationGrid = vertical(void :: animationLines ++ List(void))
        val points = zipWithMidpoints(foo.map(midpoints)).flatMap { case (xs, y) => xs.map(_ -> y) }
        points -> animationGrid
    }

    def vertical(animations : List[Animation]) : Animation = {t => x => y =>
        def makeIfs(as : List[(Animation, Int)]) : Color = as match {
            case List((a, m)) => a (t) (x) (y - m)
            case List((a1, m1), (a2, m2)) => if_(y < (m1 + 1), a1 (t) (x) (y - m1), a2 (t) (x) (y - m2))
            case more =>
                val half = more.length / 2
                val left = more.take(half)
                val (_, m) = left.last
                val right = more.drop(half)
                if_(y < (m + 1), makeIfs(left), makeIfs(right))
        }
        makeIfs(zipWithMidpoints(animations))
    }

    def horizontal(animations : List[Animation]) : Animation = {t => x => y =>
        def makeIfs(as : List[(Animation, Int)]) : Color = as match {
            case List((a, m)) => a (t) (x - m) (y)
            case List((a1, m1), (a2, m2)) => if_(x < (m1 + 1), a1 (t) (x - m1) (y), a2 (t) (x - m2) (y))
            case more =>
                val half = more.length / 2
                val left = more.take(half)
                val (_, m) = left.last
                val right = more.drop(half)
                if_(x < (m + 1), makeIfs(left), makeIfs(right))
        }
        makeIfs(zipWithMidpoints(animations))
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
