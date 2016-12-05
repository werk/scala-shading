package dk.mzw.accelemation

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import dk.mzw.accelemation.Math._
import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.{HidingDevils, Spiral, TimeLens}
import dk.mzw.accelemation.util.Prelude.simplexNoise
import dk.mzw.accelemation.util.ToHtml

object Main {

    /*
    for {(v2, v1) <- Some(1).map { case x$1@(v1) =>
        val x$2@(v2) = 4
        (x$2, x$1)
    }
    } yield v1 + v2


    val r1 : R = 1
    val r2 : R = 1
    for {(v2, v1) <- r1.map { case v1 => {
            val v2 = r2
            (v2, v1)
        }
    }
    } yield {
            v1 + v2
        }
        */

    def main(arguments : Array[String]): Unit = {

        printR(2)
        printR(2.2)
        println()

        val a : R = 1
        val b : R = 2
        val c : R = 3
        //printR(a + 1)
        printR(min(sqrt(a + b - (-c)), 20))
        println()

        printB(a === b)
        printB(a < b)
        printB(!(a >= b) || a < c)
        println()

        def useB(v : B) = v || v
        def useR(v : R) = v + v

        printB((a === b).bind(useB))
        printR(pi.bind(v => pow(v, 2) - v))
        println()

        save(TimeLens.apply, "TimeLens")
        save(HidingDevils.apply, "HidingDevils")
        save(Spiral.apply, "Spiral")
        save({t => x => y => simplexNoise(Vec3(t, x, y)) bind {i => rgba(i, i, i, 1)}}, "Hest")
    }

    def printR(e : R) = println(e)
    def printB(e : B) = println(e)

    def save(a : Animation, name : String): Unit = {
        val fileName = s"$name.html"
        val html = ToHtml(Compile(a), name)
        Files.write(Paths.get(fileName), html.getBytes(StandardCharsets.UTF_8))
    }

}
