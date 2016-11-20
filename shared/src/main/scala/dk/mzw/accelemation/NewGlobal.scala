package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.External._

object NewGlobal {

    private[NewGlobal] case class Thunk(
        original : AnyRef,
        untyped : Seq[Untyped] => Untyped,
        nameHint : String,
        signature : Seq[String],
        arguments : Seq[Untyped]
    )

    case class TermFunction[F](
        private[NewGlobal] val original : AnyRef,
        private[NewGlobal] val untyped : Seq[Untyped] => Untyped,
        private[NewGlobal] val make : Thunk => F
    ) {
        def global : F = global("f")
        def global(nameHint : String) : F = make(Thunk(original, untyped, nameHint, Seq(), Seq()))
    }

    implicit def simpleTermFunction[A <: Typed](a : A) (implicit
        bridgeA : Bridge[A]
    ) = TermFunction[A](
        original = a,
        untyped = {case Seq() => untyped(a)},
        make = {case thunk => bridgeA.make(FunctionDefinitionCall(
            definition = DomainFunctionDefinition(
                identity = thunk.original,
                signature = Signature(thunk.nameHint, bridgeA.glslTypeName, thunk.signature),
                body = thunk.untyped
            ),
            arguments = Some(thunk.arguments)
        ))}
    )

    implicit def tuple1TermFunction[A <: Typed, G](f : A => G) (implicit
        bridgeA : Bridge[A],
        gIsTermFunction : G => TermFunction[G]
    ) = TermFunction[A => G](
        original = f,
        untyped = { case a :: as => gIsTermFunction(f(bridgeA.make(a))).untyped(as)},
        make = {case thunk => a : A => gIsTermFunction(f(bridgeA.make(null))).make(thunk.copy(
            signature = thunk.signature ++ List(bridgeA.glslTypeName),
            arguments = thunk.arguments ++ List(untyped(a))
        ))}
    )

    implicit def tuple2TermFunction[A <: Typed, B <: Typed, G](f : (A, B) => G) (implicit
        bridgeA : Bridge[A],
        bridgeB : Bridge[B],
        gIsTermFunction : G => TermFunction[G]
    ) = TermFunction[(A, B) => G](
        original = f,
        untyped = { case a :: b :: as => gIsTermFunction(f(bridgeA.make(a), bridgeB.make(b))).untyped(as)},
        make = {case thunk => {case (a : A, b : B) => gIsTermFunction(f(bridgeA.make(null), bridgeB.make(null))).make(thunk.copy(
            signature = thunk.signature ++ List(bridgeA.glslTypeName, bridgeB.glslTypeName),
            arguments = thunk.arguments ++ List(untyped(a), untyped(b))
        ))}}
    )

    implicit def tuple3TermFunction[A <: Typed, B <: Typed, C <: Typed, G](f : (A, B, C) => G) (implicit
        bridgeA : Bridge[A],
        bridgeB : Bridge[B],
        bridgeC : Bridge[C],
        gIsTermFunction : G => TermFunction[G]
    ) = TermFunction[(A, B, C) => G](
        original = f,
        untyped = { case a :: b :: c :: as => gIsTermFunction(f(bridgeA.make(a), bridgeB.make(b), bridgeC.make(c))).untyped(as)},
        make = {case thunk => {case (a : A, b : B, c : C) => gIsTermFunction(f(bridgeA.make(null), bridgeB.make(null), bridgeC.make(null))).make(thunk.copy(
            signature = thunk.signature ++ List(bridgeA.glslTypeName, bridgeB.glslTypeName, bridgeC.glslTypeName),
            arguments = thunk.arguments ++ List(untyped(a), untyped(b), untyped(c))
        ))}}
    )

}