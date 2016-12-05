package dk.mzw.accelemation

import dk.mzw.accelemation.internal.Internal._
import dk.mzw.accelemation.Language._

object Global {

    private[Global] case class Thunk(
        original : AnyRef,
        untyped : Seq[Untyped] => Untyped,
        nameHint : String,
        signature : Seq[String],
        arguments : Seq[Untyped]
    )

    case class TermFunction[F](
        private[Global] val original : AnyRef,
        private[Global] val untyped : Seq[Untyped] => Untyped,
        private[Global] val make : Thunk => F
    ) {
        def global : F = global("f")
        def global(nameHint : String) : F = make(Thunk(original, untyped, nameHint, Seq(), Seq()))
    }

    implicit def simpleTermFunction[A <: Typed[A]](a : A) (implicit
        metaA : Meta[A]
    ) = TermFunction[A](
        original = a,
        untyped = {case Seq() => untyped(a)},
        make = {case thunk => metaA.make(FunctionDefinitionCall(
            definition = DomainFunctionDefinition(
                identity = thunk.original,
                signature = Signature(thunk.nameHint, metaA.glslTypeName, thunk.signature),
                body = thunk.untyped
            ),
            arguments = Some(thunk.arguments)
        ))}
    )

    implicit def tuple1TermFunction[A <: Typed[A], G](f : A => G) (implicit
        metaA : Meta[A],
        gIsTermFunction : G => TermFunction[G]
    ) = TermFunction[A => G](
        original = f,
        untyped = { case a :: as => gIsTermFunction(f(metaA.make(a))).untyped(as)},
        make = {case thunk => a : A => gIsTermFunction(f(metaA.make(null))).make(thunk.copy(
            signature = thunk.signature ++ List(metaA.glslTypeName),
            arguments = thunk.arguments ++ List(untyped(a))
        ))}
    )

    implicit def tuple2TermFunction[A <: Typed[A], B <: Typed[B], G](f : (A, B) => G) (implicit
        metaA : Meta[A],
        metaB : Meta[B],
        gIsTermFunction : G => TermFunction[G]
    ) = TermFunction[(A, B) => G](
        original = f,
        untyped = { case a :: b :: as => gIsTermFunction(f(metaA.make(a), metaB.make(b))).untyped(as)},
        make = {case thunk => {case (a : A, b : B) => gIsTermFunction(f(metaA.make(null), metaB.make(null))).make(thunk.copy(
            signature = thunk.signature ++ List(metaA.glslTypeName, metaB.glslTypeName),
            arguments = thunk.arguments ++ List(untyped(a), untyped(b))
        ))}}
    )

    implicit def tuple3TermFunction[A <: Typed[A], B <: Typed[B], C <: Typed[C], G](f : (A, B, C) => G) (implicit
        metaA : Meta[A],
        metaB : Meta[B],
        metaC : Meta[C],
        gIsTermFunction : G => TermFunction[G]
    ) = TermFunction[(A, B, C) => G](
        original = f,
        untyped = { case a :: b :: c :: as => gIsTermFunction(f(metaA.make(a), metaB.make(b), metaC.make(c))).untyped(as)},
        make = {case thunk => {case (a : A, b : B, c : C) => gIsTermFunction(f(metaA.make(null), metaB.make(null), metaC.make(null))).make(thunk.copy(
            signature = thunk.signature ++ List(metaA.glslTypeName, metaB.glslTypeName, metaC.glslTypeName),
            arguments = thunk.arguments ++ List(untyped(a), untyped(b), untyped(c))
        ))}}
    )

    implicit def tuple4TermFunction[A <: Typed[A], B <: Typed[B], C <: Typed[C], D <: Typed[D], G](f : (A, B, C, D) => G) (implicit
        metaA : Meta[A],
        metaB : Meta[B],
        metaC : Meta[C],
        metaD : Meta[D],
        gIsTermFunction : G => TermFunction[G]
    ) = TermFunction[(A, B, C, D) => G](
        original = f,
        untyped = { case a :: b :: c :: d :: as => gIsTermFunction(f(metaA.make(a), metaB.make(b), metaC.make(c), metaD.make(d))).untyped(as)},
        make = {case thunk => {case (a : A, b : B, c : C, d : D) => gIsTermFunction(f(metaA.make(null), metaB.make(null), metaC.make(null), metaD.make(null))).make(thunk.copy(
            signature = thunk.signature ++ List(metaA.glslTypeName, metaB.glslTypeName, metaC.glslTypeName, metaD.glslTypeName),
            arguments = thunk.arguments ++ List(untyped(a), untyped(b), untyped(c), untyped(d))
        ))}}
    )

    // tuple5TermFunction .. tuple22TermFunction
    // TODO: there must be a better way to write this

    implicit def tuple7TermFunction[
    A1 <: Typed[A1],
    A2 <: Typed[A2],
    A3 <: Typed[A3],
    A4 <: Typed[A4],
    A5 <: Typed[A5],
    A6 <: Typed[A6],
    A7 <: Typed[A7],
    R
    ](f : (A1, A2, A3, A4, A5, A6, A7) => R) (implicit
        metaA1 : Meta[A1],
        metaA2 : Meta[A2],
        metaA3 : Meta[A3],
        metaA4 : Meta[A4],
        metaA5 : Meta[A5],
        metaA6 : Meta[A6],
        metaA7 : Meta[A7],
        rIsTermFunction : R => TermFunction[R]
    ) = TermFunction[(A1, A2, A3, A4, A5, A6, A7) => R](
        original = f,
        untyped = { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: as => rIsTermFunction(f(metaA1.make(a1), metaA2.make(a2), metaA3.make(a3), metaA4.make(a4), metaA5.make(a5), metaA6.make(a6), metaA7.make(a7))).untyped(as)},
        make = {case thunk => {case (a1 : A1, a2 : A2, a3 : A3, a4 : A4, a5 : A5, a6 : A6, a7 : A7) => rIsTermFunction(f(metaA1.make(null), metaA2.make(null), metaA3.make(null), metaA4.make(null), metaA5.make(null), metaA6.make(null), metaA7.make(null))).make(thunk.copy(
            signature = thunk.signature ++ List(metaA1.glslTypeName, metaA2.glslTypeName, metaA3.glslTypeName, metaA4.glslTypeName, metaA5.glslTypeName, metaA6.glslTypeName, metaA7.glslTypeName),
            arguments = thunk.arguments ++ List(untyped(a1), untyped(a2), untyped(a3), untyped(a4), untyped(a5), untyped(a6), untyped(a7))
        ))}}
    )
}