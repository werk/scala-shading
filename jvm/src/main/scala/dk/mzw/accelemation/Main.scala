package dk.mzw.accelemation

import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Path, Files}

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Language.Math._

object Main {

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

        save(TimeLens)
        save(HidingDevils)
        save(Spiral)
    }

    def printR(e : R) = println(e)
    def printB(e : B) = println(e)

    def save(o : {def apply : Animation}): Unit = {
        //val fileName = s"${o.getClass.getSimpleName.filter(_.isLetterOrDigit)}.html"
        val fileName = s"foo.html"
        val html = ToHtml(o.apply)
        println(html)
        Files.write(Paths.get(fileName), html.getBytes(StandardCharsets.UTF_8))
    }

}