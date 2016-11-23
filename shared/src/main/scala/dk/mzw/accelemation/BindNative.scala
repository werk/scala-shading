package dk.mzw.accelemation

import dk.mzw.accelemation.External._
import dk.mzw.accelemation.Internal.{FunctionDefinitionCall, NativeFunctionDefinition}

object BindNative {
    // Bind foreign functions

    def bindNativeConstant[A](source : String)(implicit
        bridgeA : Bridge[A]
    ) : A = {bridgeA.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = bridgeA.glslTypeName,
            argumentTypes = Seq()
        ),
        arguments = None
    ))}

    def bindNative0[A](source : String)(implicit
        bridgeA : Bridge[A]
    ) : A = {bridgeA.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = bridgeA.glslTypeName,
            argumentTypes = Seq()
        ),
        arguments = Some(Seq())
    ))}
    def bindNative1[A1, A2](source : String)(implicit
        bridgeA1 : Bridge[A1],
        bridgeA2 : Bridge[A2]
    ) : A1 => A2 = {a1 : A1 => bridgeA2.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = bridgeA2.glslTypeName,
            argumentTypes = Seq(bridgeA1.glslTypeName)
        ),
        arguments = Some(Seq(bridgeA1.untyped(a1)))
    ))}

    def bindNative2[A1, A2, A3](source : String)(implicit
        bridgeA1 : Bridge[A1],
        bridgeA2 : Bridge[A2],
        bridgeA3 : Bridge[A3]
    ) : A1 => A2 => A3 = {a1 : A1 => a2 : A2 => bridgeA3.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = bridgeA3.glslTypeName,
            argumentTypes = Seq(bridgeA1.glslTypeName, bridgeA2.glslTypeName)
        ),
        arguments = Some(Seq(bridgeA1.untyped(a1), bridgeA2.untyped(a2)))
    ))}

    def bindNative4[A1, A2, A3, A4, A5](source : String)(implicit
        bridgeA1 : Bridge[A1],
        bridgeA2 : Bridge[A2],
        bridgeA3 : Bridge[A3],
        bridgeA4 : Bridge[A4],
        bridgeA5 : Bridge[A5]
    ) : A1 => A2 => A3 => A4 => A5 = {a1 : A1 => a2 : A2 => a3 : A3 => a4 : A4 => bridgeA5.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = bridgeA5.glslTypeName,
            argumentTypes = Seq(bridgeA1.glslTypeName, bridgeA2.glslTypeName, bridgeA3.glslTypeName, bridgeA4.glslTypeName)
        ),
        arguments = Some(Seq(bridgeA1.untyped(a1), bridgeA2.untyped(a2), bridgeA3.untyped(a3), bridgeA4.untyped(a4)))
    ))}

}
