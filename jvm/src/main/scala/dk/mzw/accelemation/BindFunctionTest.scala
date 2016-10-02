package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.{HidingDevils, Spiral, TimeLens}
import CompileFunction.{F3WithOperations, bind3}

import scala.collection.mutable

object BindFunctionTest {

    //def signature[F](implicit a : Fun[F]) = a.typeNames.mkString(" -> ")

    def plus (a : R) (b : R) = a + b

    val plusBound : R => R => R = plus


    def main(args: Array[String]) {
        val spiral = bind3("spiral", Spiral.apply)

        val timeLens2 : Animation = { t => x => y =>
            spiral(t, x, y) bind { s =>
                TimeLens.apply(t)(x)(y)
            }
        }

        val time_lens = bind3("time_lens", timeLens2)
        val hiding_devils = bind3("hiding_devils", HidingDevils.apply)
        val x = time_lens(1, 2, 3)
        println(x)

        val animation : Animation = {t => x => y =>
            time_lens(t / 2, 1 + x, y - 1) bind {tl =>
                hiding_devils(t, x, y) bind { hd =>
                    spiral(t, x, y) bind { s =>
                        vec4(tl.magnitude, hd.magnitude, s.magnitude, 0)
                    }
                }
            }
        }

        val source = CompileFunction.compileAnimationWithDependencies(animation).mkString("\n")
        println(source)
    }
}
