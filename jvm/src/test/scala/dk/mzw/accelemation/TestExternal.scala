package dk.mzw.accelemation

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Math._

object TestExternal {
    def main(args: Array[String]) {
        val r1 = R(1)
        val r2 = R(2)
        val vec2 = Vec2(r1, r2)

        val x = vec2.normalize
        val r3 = r1 + r2
        val r4 = 1 + r3
        val r5 = r3 + 1
        val r6 = 1.1 + r3
        val r7 = r3 + 1.1

        val vec2b = Vec2(sin(r3), r1)
        val vec2c = vec2b.bind{ v => v.yx}

        println(untyped(vec2c.xx))

    }

}
