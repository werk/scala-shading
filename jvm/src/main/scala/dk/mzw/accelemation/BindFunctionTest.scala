package dk.mzw.accelemation

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.{HidingDevils, Spiral, TimeLens}

object BindFunctionTest {

    def main(args: Array[String]) {
        def plus(a : R) (b : R) = a + b
        val plus2 = plus _
        val plus3 = {a : R => b : R => a + b}

        val inc = bindNative1[Double, Double]("""
            float inc(float x) {
                return x + 1
            }
        """)

        val plus4 = bindNative2[Double, Double, Double]("""
            float plus(float a1, float a2) {
                return a1 + a2;
            }
        """)

        val plusBuild = bind2(plus, "plus")
        val plusBuild2 = bind2(plus2, "plus2")
        val plusBuild3 = bind2(plus3, "plus3")

        val spiral = bind3(Spiral.apply, "spiral")

        val timeLens2 : Animation = { t => x => y =>
            spiral(t)(x)(y) bind { s =>
                TimeLens.apply(t)(x)(y)
            }
        }

        val time_lens = bind3(timeLens2, "time_lens")
        val hiding_devils = bind3(HidingDevils.apply, "hiding_devils")
        val x = time_lens(1)(2)(3)
        println(x)

        val animation : Animation = {t => x => y =>
            time_lens(t / 2)(1 + x)(y - 1) bind {tl =>
                hiding_devils(t)(x)(y) bind { hd =>
                    spiral(t)(x)(y) bind { s =>
                        vec4(plusBuild(plusBuild2(tl.magnitude)(42))(12), plusBuild3(hd.magnitude)(inc(plus4(1337)(17))), s.magnitude, 0)
                    }
                }
            }
        }

        val sourceAndUniforms = Compile.apply(animation, "animation", "t", "x", "y")
        println(sourceAndUniforms.source)
    }
}
