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

    def bind3(f : R => R => R => Vec4) : R => R => R => Vec4 = F3(f)

    def bind32(name : String, f : R => R => R => Vec4)(implicit a : Fun[R => R => R => Vec4]) : Term[R => R => R => Vec4] = {
        val List(t1, t2, t3, t4) = a.typeNames
        def fu(a1 : Untyped) (a2 : Untyped) (a3 : Untyped) : Untyped = f(Term(a1)) (Term(a2)) (Term(a3)).untyped
        Term(BindF3(name, fu, t1, t2, t3, t4))
    }

    implicit class F3WithOperations(a : Term[R => R => R => Vec4]){
        def apply(a1 : R, a2 : R, a3 : R) : Vec4 = Term(CallF3(a.untyped, a1.untyped, a2.untyped, a3.untyped))
    }

    case class F3(f : R => R => R => Vec4) extends Function[R, Function[R, Function[R, Vec4]]]{
        def apply(a : R) : R => R => Vec4 = f (a)
    }

    def main(args: Array[String]) {
        val a = bind32("TimeLens", TimeLens.apply)
        val x = a(1, 2, 3)
        println(x)

        //val source = ToGlsl(bind3(TimeLens.apply))
        //println(source)
    }
}
