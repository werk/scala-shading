package dk.mzw.accelemation

import dk.mzw.accelemation.internal.Internal._
import dk.mzw.accelemation.Language._

/**
  * .global - Extension method for functions operating on Typed values
  *
  * Tell the compiler that a Scala function should be declared (and reused) as a top-level function in GLSL.
  */
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
        private[Global] val build : Thunk => F
    ) {
        def global : F = global("f")
        def global(nameHint : String) : F = build(Thunk(original, untyped, nameHint, Seq(), Seq()))
    }

    implicit def simpleTermFunction[A <: Typed[A]](a : A) (implicit
        m : Meta[A]
    ) = TermFunction[A](
        original = a,
        untyped = {case Seq() => untyped(a)},
        build = {thunk => m.make(FunctionDefinitionCall(
            definition = DomainFunctionDefinition(
                identity = thunk.original,
                signature = Signature(thunk.nameHint, m.t, thunk.signature),
                body = thunk.untyped
            ),
            arguments = Some(thunk.arguments)
        ))}
    )

    implicit def tuple1TermFunction[A, R](f : A => R) (implicit
        metaA : Meta[A],
        termFunction : R => TermFunction[R]
    ) = TermFunction[A => R](
        original = f,
        untyped = {case a :: as => termFunction(f(metaA.make(a))).untyped(as)},
        build = {thunk => a => termFunction(f(metaA.make(null))).build(thunk.copy(
            signature = thunk.signature ++ List(metaA.t),
            arguments = thunk.arguments ++ List(metaA.untyped(a))
        ))}
    )

    implicit def tuple2TermFunction[A1, A2, R](f : (A1, A2) => R) (implicit
        m1 : Meta[A1],
        m2 : Meta[A2],
        termFunction : R => TermFunction[R]
    ) = TermFunction[(A1, A2) => R](
        original = f,
        untyped = {case a1 :: a2 :: as => termFunction(f(m1.make(a1), m2.make(a2))).untyped(as)},
        build = {thunk => (a1, a2) => termFunction(f(m1.make(null), m2.make(null))).build(thunk.copy(
            signature = thunk.signature ++ List(m1.t, m2.t),
            arguments = thunk.arguments ++ List(m1.untyped(a1), m2.untyped(a2))
        ))}
    )

    implicit def tuple3TermFunction[A1, A2, A3, R](f : (A1, A2, A3) => R) (implicit
        m1 : Meta[A1],
        m2 : Meta[A2],
        m3 : Meta[A3],
        termFunction : R => TermFunction[R]
    ) = TermFunction[(A1, A2, A3) => R](
        original = f,
        untyped = {case a1 :: a2 :: a3 :: as => termFunction(f(m1.make(a1), m2.make(a2), m3.make(a3))).untyped(as)},
        build = {thunk => (a1, a2, a3) => termFunction(f(m1.make(null), m2.make(null), m3.make(null))).build(thunk.copy(
            signature = thunk.signature ++ List(m1.t, m2.t, m3.t),
            arguments = thunk.arguments ++ List(m1.untyped(a1), m2.untyped(a2), m3.untyped(a3))
        ))}
    )

    implicit def tuple4TermFunction[A1, A2, A3, A4, R](f : (A1, A2, A3, A4) => R) (implicit
        m1 : Meta[A1], m2 : Meta[A2], m3 : Meta[A3], m4 : Meta[A4],
        termFunction : R => TermFunction[R]
    ) = TermFunction[(A1, A2, A3, A4) => R](
        original = f,
        untyped = {case a1 :: a2 :: a3 :: a4 :: as => termFunction(f(m1.make(a1), m2.make(a2), m3.make(a3), m4.make(a4))).untyped(as)},
        build = {thunk => (a1, a2, a3, a4) => termFunction(f(m1.make(null), m2.make(null), m3.make(null), m4.make(null))).build(thunk.copy(
            signature = thunk.signature ++ List(m1.t, m2.t, m3.t, m4.t),
            arguments = thunk.arguments ++ List(m1.untyped(a1), m2.untyped(a2), m3.untyped(a3), m4.untyped(a4))
        ))}
    )

    implicit def tuple5TermFunction[A1, A2, A3, A4, A5, R](f : (A1, A2, A3, A4, A5) => R) (implicit
        m1 : Meta[A1], m2 : Meta[A2], m3 : Meta[A3], m4 : Meta[A4], m5 : Meta[A5],
        termFunction : R => TermFunction[R]
    ) = TermFunction[(A1, A2, A3, A4, A5) => R](
        original = f,
        untyped = {case a1 :: a2 :: a3 :: a4 :: a5 :: as => termFunction(f(m1.make(a1), m2.make(a2), m3.make(a3), m4.make(a4), m5.make(a5))).untyped(as)},
        build = {thunk => (a1, a2, a3, a4, a5) => termFunction(f(m1.make(null), m2.make(null), m3.make(null), m4.make(null), m5.make(null))).build(thunk.copy(
            signature = thunk.signature ++ List(m1.t, m2.t, m3.t, m4.t, m5.t),
            arguments = thunk.arguments ++ List(m1.untyped(a1), m2.untyped(a2), m3.untyped(a3), m4.untyped(a4), m5.untyped(a5))
        ))}
    )

    implicit def tuple6TermFunction[A1, A2, A3, A4, A5, A6, R](f : (A1, A2, A3, A4, A5, A6) => R) (implicit
        m1 : Meta[A1], m2 : Meta[A2], m3 : Meta[A3], m4 : Meta[A4], m5 : Meta[A5], m6 : Meta[A6],
        termFunction : R => TermFunction[R]
    ) = TermFunction[(A1, A2, A3, A4, A5, A6) => R](
        original = f,
        untyped = {case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: as => termFunction(f(m1.make(a1), m2.make(a2), m3.make(a3), m4.make(a4), m5.make(a5), m6.make(a6))).untyped(as)},
        build = {thunk => (a1, a2, a3, a4, a5, a6) => termFunction(f(m1.make(null), m2.make(null), m3.make(null), m4.make(null), m5.make(null), m6.make(null))).build(thunk.copy(
            signature = thunk.signature ++ List(m1.t, m2.t, m3.t, m4.t, m5.t, m6.t),
            arguments = thunk.arguments ++ List(m1.untyped(a1), m2.untyped(a2), m3.untyped(a3), m4.untyped(a4), m5.untyped(a5), m6.untyped(a6))
        ))}
    )

    implicit def tuple7TermFunction[A1, A2, A3, A4, A5, A6, A7, R](f : (A1, A2, A3, A4, A5, A6, A7) => R) (implicit
        m1 : Meta[A1], m2 : Meta[A2], m3 : Meta[A3], m4 : Meta[A4], m5 : Meta[A5], m6 : Meta[A6], m7 : Meta[A7],
        termFunction : R => TermFunction[R]
    ) = TermFunction[(A1, A2, A3, A4, A5, A6, A7) => R](
        original = f,
        untyped = {case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: as => termFunction(f(m1.make(a1), m2.make(a2), m3.make(a3), m4.make(a4), m5.make(a5), m6.make(a6), m7.make(a7))).untyped(as)},
        build = {thunk => (a1, a2, a3, a4, a5, a6, a7) => termFunction(f(m1.make(null), m2.make(null), m3.make(null), m4.make(null), m5.make(null), m6.make(null), m7.make(null))).build(thunk.copy(
            signature = thunk.signature ++ List(m1.t, m2.t, m3.t, m4.t, m5.t, m6.t, m7.t),
            arguments = thunk.arguments ++ List(m1.untyped(a1), m2.untyped(a2), m3.untyped(a3), m4.untyped(a4), m5.untyped(a5), m6.untyped(a6), m7.untyped(a7))
        ))}
    )

    // TODO: .. tuple22TermFunction - there must be a better way to write this

}