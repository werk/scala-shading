package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language._

object TypeClassTest {

    case class TermFunction(
        untyped : Seq[Untyped] => Untyped,
        signature : List[String]
    )
    implicit def simpleTermFunction[A, B](f : Term[A] => Term[B]) (implicit
        typeA : VariableType[Term[A]],
        typeB : VariableType[Term[B]]
    ) : TermFunction = TermFunction(
        untyped = {case Seq(a) => f(Term(a)).untyped},
        signature = List(typeA.t, typeB.t)
    )
    implicit def curriedTermFunction[A, G](f : Term[A] => G) (implicit
        typeA : VariableType[Term[A]],
        gIsTermFunction : G => TermFunction
    ) : TermFunction = TermFunction(
        untyped = { case a :: as => gIsTermFunction(f(Term(a))).untyped(as)},
        signature = typeA.t :: gIsTermFunction(f(Term(null))).signature
    )

    def global[F](f : F)(implicit isTermFunction : F => TermFunction) : F = {
        ???
    }

    def inc(n : R) = 1 + n
    def plus(a : R) (b : R) = a + b

    val f1 : R => R = {a => 1 * a}
    val f2 : R => R => R = {a => b => a * b}
    val f3 : R => R => R => R = {a => b => c => a * b - c}

    def main(args: Array[String]) {
        println(f1.untyped(Seq(1.untyped)))
        println(f2.untyped(Seq(1.untyped, 2.untyped)))
        println(f3.untyped(Seq(1.untyped, 2.untyped, 3.untyped)))

        println(f1.signature)
        println(curriedTermFunction(f2).signature)
        println(f3.signature)

        println(inc _ untyped Seq(1.untyped) )
        println(plus _ untyped(Seq(1.untyped, 2.untyped)))
    }

}