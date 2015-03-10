package dk.mzw.accelemation

import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Path, Files}

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Language.Math._

object LanguageTest {

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
        printR(pi.bind(v => v**2 - v))
        println()

        def animation(t : Time) (x : R) (y : R) = hsva(x, y, sin(t), 1)
        val html = ToHtml(animation)
        println(html)
        Files.write(Paths.get("foo.html"), html.getBytes(StandardCharsets.UTF_8))
    }

    def printR(e : R) = println(e)
    def printB(e : B) = println(e)

}
