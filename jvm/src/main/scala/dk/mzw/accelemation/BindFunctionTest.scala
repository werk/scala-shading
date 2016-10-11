package dk.mzw.accelemation

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.{HidingDevils, Spiral, TimeLens}

object BindFunctionTest {

    def plus (a : R) (b : R) = a + b

    val plusBound : R => R => R = plus


    def main(args: Array[String]) {
        def plus(a : R) (b : R) = a + b
        val plusBuild = bind2(plus, "plus")
        val plusBuild2 = bind2(plus, "plus")

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
                        vec4(plusBuild(tl.magnitude)(42), plusBuild2(hd.magnitude)(1337), s.magnitude, 0)
                    }
                }
            }
        }

        val sourceAndUniforms = CompileFunction.compileAnimationWithDependencies(animation, "animation", "t", "x", "y")
        println(sourceAndUniforms.source)
    }
}
