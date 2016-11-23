package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language._

object TermGlobal {

    private[TermGlobal] case class Thunk(
        original : AnyRef,
        untyped : Seq[Untyped] => Untyped,
        nameHint : String,
        signature : Seq[String],
        arguments : Seq[Untyped]
    )

    case class TermFunction[F](
        private[TermGlobal] val original : AnyRef,
        private[TermGlobal] val untyped : Seq[Untyped] => Untyped,
        private[TermGlobal] val make : Thunk => F
    ) {
        def global : F = global("f")
        def global(nameHint : String) : F = make(Thunk(original, untyped, nameHint, Seq(), Seq()))
    }

    implicit def simpleTermFunction[A](a : Term[A]) (implicit
        typeA : VariableType[Term[A]]
    ) = TermFunction[Term[A]](
        original = a,
        untyped = {case Seq() => a.untyped},
        make = {case thunk => Term[A](FunctionDefinitionCall(
            definition = DomainFunctionDefinition(
                identity = thunk.original,
                signature = Signature(thunk.nameHint, typeA.t, thunk.signature),
                body = thunk.untyped
            ),
            arguments = Some(thunk.arguments)
        ))}
    )

    implicit def tuple1TermFunction[A, G](f : Term[A] => G) (implicit
        typeA : VariableType[Term[A]],
        gIsTermFunction : G => TermFunction[G]
    ) = TermFunction[Term[A] => G](
        original = f,
        untyped = { case a :: as => gIsTermFunction(f(Term(a))).untyped(as)},
        make = {case thunk => a : Term[A] => gIsTermFunction(f(Term(null))).make(thunk.copy(
            signature = thunk.signature ++ List(typeA.t),
            arguments = thunk.arguments ++ List(a.untyped)
        ))}
    )

    implicit def tuple2TermFunction[A, B, G](f : (Term[A], Term[B]) => G) (implicit
        typeA : VariableType[Term[A]],
        typeB : VariableType[Term[B]],
        gIsTermFunction : G => TermFunction[G]
    ) = TermFunction[(Term[A], Term[B]) => G](
        original = f,
        untyped = { case a :: b :: as => gIsTermFunction(f(Term(a), Term(b))).untyped(as)},
        make = {case thunk => {case (a : Term[A], b : Term[B]) => gIsTermFunction(f(Term(null), Term(null))).make(thunk.copy(
            signature = thunk.signature ++ List(typeA.t, typeB.t),
            arguments = thunk.arguments ++ List(a.untyped, b.untyped)
        ))}}
    )

    implicit def tuple3TermFunction[A, B, C, G](f : (Term[A], Term[B], Term[C]) => G) (implicit
        typeA : VariableType[Term[A]],
        typeB : VariableType[Term[B]],
        typeC : VariableType[Term[C]],
        gIsTermFunction : G => TermFunction[G]
    ) = TermFunction[(Term[A], Term[B], Term[C]) => G](
        original = f,
        untyped = { case a :: b :: c :: as => gIsTermFunction(f(Term(a), Term(b), Term(c))).untyped(as)},
        make = {case thunk => {case (a : Term[A], b : Term[B], c : Term[C]) => gIsTermFunction(f(Term(null), Term(null), Term(null))).make(thunk.copy(
            signature = thunk.signature ++ List(typeA.t, typeB.t, typeC.t),
            arguments = thunk.arguments ++ List(a.untyped, b.untyped, c.untyped)
        ))}}
    )

}