package dk.mzw.accelemation

import dk.mzw.accelemation.Typed._
import dk.mzw.accelemation.BuildInFunctions._

object TestTyped {
    def main(args: Array[String]) {
        val r1 = R(1)
        val r2 = R(2)
        val vec2 = Vec2(r1, r2)

        val x = vec2.normalize
        val r3 : R = r1 *r2

        println(sin(r3))

    }

}
