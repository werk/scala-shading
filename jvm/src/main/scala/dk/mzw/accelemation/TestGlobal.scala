package dk.mzw.accelemation

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Global._

object TestGlobal {

    def main(args: Array[String]) {
        val f1 : R => R = {a => 1 * a}
        val f2 : R => R => R = {a => b => a * b}
        val f3 : R => R => R => R = {a => b => c => a * b - c}

        println(f1.global(1))
        println(f2.global(1)(2))
        println(f3.global("foobar3")(1)(2)(3))

        generate(2, "xyzw", "rgba", "stpq")
        generate(3, "xyzw", "rgba", "stpq")
        generate(4, "xyzw", "rgba", "stpq")

        def generate(n : Int, axesList : String*): Unit = {
            def line(a : String) = s"""        def $a : Self$n = make$n(Field("$a", untyped))"""
            def block(axes : String) : Unit = {
                val letters = axes.take(n)
                for{
                    a <- letters
                } println(line(s"$a"))
                println()

                if(n < 2) return
                for{
                    a <- letters
                    b <- letters
                } println(line(s"$a$b"))
                println()

                if(n < 3) return
                for{
                    a <- letters
                    b <- letters
                    c <- letters
                } println(line(s"$a$b$c"))
                println()

                if(n < 4) return
                for{
                    a <- letters
                    b <- letters
                    c <- letters
                    d <- letters
                } println(line(s"$a$b$c$d"))
                println()
            }

            println(s"    sealed trait XVec$n extends XVec {")
            axesList.foreach(block)
            println(s"    }")
            println()
        }

    }
}
