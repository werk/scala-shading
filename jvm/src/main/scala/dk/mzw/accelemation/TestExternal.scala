package dk.mzw.accelemation

import dk.mzw.accelemation.External._
import dk.mzw.accelemation.BuildInFunctions._

object TestExternal {
    def main(args: Array[String]) {
        val r1 = R(1)
        val r2 = R(2)
        val vec2 = Vec2(r1, r2)

        val x = vec2.normalize
        val r3 : R = r1 + r2
        val r4 : R = 1 + r3
        val r5 : R = r3 + 1
        val r6 : R = 1.1 + r3
        val r7 : R = r3 + 1.1

        val vec2b : Vec2 = Vec2(sin(r3), r1)
        println(untyped(vec2b))

    }

}
