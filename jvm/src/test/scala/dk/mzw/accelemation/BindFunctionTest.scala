package dk.mzw.accelemation

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Global._
import dk.mzw.accelemation.BindNative._
import dk.mzw.accelemation.samples.{HidingDevils, Spiral, TimeLens}

object BindFunctionTest {

    def main(args: Array[String]) {
        def plus(a : R) (b : R) = a + b
        val plus2 = plus _
        val plus3 = {a : R => b : R => a + b}

        val inc = bindNative1[R, R]("""
            float inc(float x) {
                return x + 1
            }
        """)

        val plus4 = bindNative2[R, R, R]("""
            float plus(float a1, float a3) {
                return a1 + a3;
            }
        """)

        val plusBuild = (plus _).global("plus")
        val plusBuild2 = plus2.global("plus2")
        val plusBuild3 = plus3.global("plus3")

        val spiral = Spiral.apply.global("spiral")

        val timeLens2 : Animation = { t => x => y =>
            spiral(t)(x)(y) bind { s =>
                TimeLens.apply(t)(x)(y)
            }
        }

        val time_lens = timeLens2.global("time_lens")
        val hiding_devils = HidingDevils.apply.global("hiding_devils")
        val x = time_lens(1)(2)(3)
        println(x)

        val animation : Animation = {t => x => y =>
            time_lens(t / 2)(1 + x)(y - 1) bind {tl =>
                hiding_devils(t)(x)(y) bind { hd =>
                    spiral(t)(x)(y) bind { s =>
                        Vec4(plusBuild(plusBuild2(tl.magnitude)(42))(12), plusBuild3(hd.magnitude)(inc(plus4(1337)(17))), s.magnitude, 0)
                    }
                }
            }
        }

        println(Compile(animation))
    }
}
