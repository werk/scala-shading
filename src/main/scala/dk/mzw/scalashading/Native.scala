package dk.mzw.scalashading

import dk.mzw.scalashading.Language._
import dk.mzw.scalashading.internal.Internal._

/**
  * Native GLSL function interface
  */
object Native {

    def wrapNativeConstant[A](source : String)(implicit
        metaA : Meta[A]
    ) : A = {metaA.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaA.t,
            argumentTypes = Seq()
        ),
        arguments = None
    ))}

    def wrapNative0[R](source : String)(implicit
        metaR : Meta[R]
    ) : R = {metaR.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaR.t,
            argumentTypes = Seq()
        ),
        arguments = Some(Seq())
    ))}

    def wrapNative1[A1, R](source : String)(implicit
        metaA1 : Meta[A1], metaR : Meta[R]
    ) : A1 => R = {a1 : A1 => metaR.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaR.t,
            argumentTypes = Seq(metaA1.t)
        ),
        arguments = Some(Seq(metaA1.untyped(a1)))
    ))}

    def wrapNative2[A1, A2, R](source : String)(implicit
        metaA1 : Meta[A1], metaA2 : Meta[A2], metaR : Meta[R]
    ) : (A1, A2) => R = {(a1, a2) => metaR.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaR.t,
            argumentTypes = Seq(metaA1.t, metaA2.t)
        ),
        arguments = Some(Seq(metaA1.untyped(a1), metaA2.untyped(a2)))
    ))}

    def wrapNative3[A1, A2, A3, R](source : String)(implicit
        metaA1 : Meta[A1], metaA2 : Meta[A2], metaA3 : Meta[A3], metaR : Meta[R]
    ) : (A1, A2, A3) => R = {(a1, a2, a3) => metaR.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaR.t,
            argumentTypes = Seq(metaA1.t, metaA2.t, metaA3.t)
        ),
        arguments = Some(Seq(metaA1.untyped(a1), metaA2.untyped(a2), metaA3.untyped(a3)))
    ))}

    def wrapNative4[A1, A2, A3, A4, R](source : String)(implicit
        metaA1 : Meta[A1], metaA2 : Meta[A2], metaA3 : Meta[A3], metaA4 : Meta[A4], metaR : Meta[R]
    ) : (A1, A2, A3, A4) => R = {(a1, a2, a3, a4) => metaR.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaR.t,
            argumentTypes = Seq(metaA1.t, metaA2.t, metaA3.t, metaA4.t)
        ),
        arguments = Some(Seq(metaA1.untyped(a1), metaA2.untyped(a2), metaA3.untyped(a3), metaA4.untyped(a4)))
    ))}

    def wrapNative5[A1, A2, A3, A4, A5, R](source : String)(implicit
        metaA1 : Meta[A1], metaA2 : Meta[A2], metaA3 : Meta[A3], metaA4 : Meta[A4], metaA5 : Meta[A5], metaR : Meta[R]
    ) : (A1, A2, A3, A4, A5) => R = {(a1, a2, a3, a4, a5) => metaR.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaR.t,
            argumentTypes = Seq(metaA1.t, metaA2.t, metaA3.t, metaA4.t, metaA5.t)
        ),
        arguments = Some(Seq(metaA1.untyped(a1), metaA2.untyped(a2), metaA3.untyped(a3), metaA4.untyped(a4), metaA5.untyped(a5)))
    ))}

    def wrapNative6[A1, A2, A3, A4, A5, A6, R](source : String)(implicit
        metaA1 : Meta[A1], metaA2 : Meta[A2], metaA3 : Meta[A3], metaA4 : Meta[A4], metaA5 : Meta[A5], metaA6 : Meta[A6], metaR : Meta[R]
    ) : (A1, A2, A3, A4, A5, A6) => R = {(a1, a2, a3, a4, a5, a6) => metaR.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaR.t,
            argumentTypes = Seq(metaA1.t, metaA2.t, metaA3.t, metaA4.t, metaA5.t, metaA6.t)
        ),
        arguments = Some(Seq(metaA1.untyped(a1), metaA2.untyped(a2), metaA3.untyped(a3), metaA4.untyped(a4), metaA5.untyped(a5), metaA6.untyped(a6)))
    ))}

    def wrapNative7[A1, A2, A3, A4, A5, A6, A7, R](source : String)(implicit
        metaA1 : Meta[A1], metaA2 : Meta[A2], metaA3 : Meta[A3], metaA4 : Meta[A4], metaA5 : Meta[A5], metaA6 : Meta[A6], metaA7 : Meta[A7], metaR : Meta[R]
    ) : (A1, A2, A3, A4, A5, A6, A7) => R = {(a1, a2, a3, a4, a5, a6, a7) => metaR.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaR.t,
            argumentTypes = Seq(metaA1.t, metaA2.t, metaA3.t, metaA4.t, metaA5.t, metaA6.t, metaA7.t)
        ),
        arguments = Some(Seq(metaA1.untyped(a1), metaA2.untyped(a2), metaA3.untyped(a3), metaA4.untyped(a4), metaA5.untyped(a5), metaA6.untyped(a6), metaA7.untyped(a7)))
    ))}
}
