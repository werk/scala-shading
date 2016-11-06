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

        for{
            a <- 'x' to 'z'
            b <- 'x' to 'z'
        } {
            println(s"""def $a$b : Vec3 = Term(Field("$a$b", v.untyped))""")
        }

        for{
            a <- 'x' to 'z'
            b <- 'x' to 'z'
            c <- 'x' to 'z'
        } {
            println(s"""def $a$b$c : Vec3 = Term(Field("$a$b$c", v.untyped))""")
        }
    }
}
