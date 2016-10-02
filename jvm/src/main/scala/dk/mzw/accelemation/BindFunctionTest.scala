package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.TimeLens

object BindFunctionTest {

    case class Fun[F](typeNames : List[String])
    implicit def functionPrimitive[P](implicit a : VariableType[P]) = Fun[P](List(a.t))
    implicit def functionArrow[A, F](implicit a : VariableType[A], f : Fun[F]) = Fun[A => F](a.t :: f.typeNames)

    def signature[F](implicit a : Fun[F]) = a.typeNames.mkString(" -> ")

    def plus (a : R) (b : R) = a + b

    val plusBound : R => R => R = plus

    def bind3(name : String, f : R => R => R => Vec4)(implicit a : Fun[R => R => R => Vec4]) : Term[R => R => R => Vec4] = {
        val List(t1, t2, t3, t4) = a.typeNames
        def fu(a1 : Untyped) (a2 : Untyped) (a3 : Untyped) : Untyped = f(Term(a1)) (Term(a2)) (Term(a3)).untyped
        Term(BindF3(name, fu, t1, t2, t3, t4))
    }

    implicit class F3WithOperations(a : Term[R => R => R => Vec4]){
        def apply(a1 : R, a2 : R, a3 : R) : Vec4 = Term(CallF3(a.untyped, a1.untyped, a2.untyped, a3.untyped))
    }

    def main(args: Array[String]) {
        val a = bind3("time_lens", TimeLens.apply)
        val x = a(1, 2, 3)
        println(x)

        val animation : Animation = {t => x => y =>
            a(t / 2, 1 + x, y - 1)
        }

        val source = CompileFunction.compileAnimationWithDep(animation).source
        println(source)
    }
}
