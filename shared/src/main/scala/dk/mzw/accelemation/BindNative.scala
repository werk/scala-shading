package dk.mzw.accelemation

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.internal.Internal._

object BindNative {
    // Bind foreign functions

    def bindNativeConstant[A](source : String)(implicit
        metaA : Meta[A]
    ) : A = {metaA.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaA.glslTypeName,
            argumentTypes = Seq()
        ),
        arguments = None
    ))}

    def bindNative0[A](source : String)(implicit
        metaA : Meta[A]
    ) : A = {metaA.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaA.glslTypeName,
            argumentTypes = Seq()
        ),
        arguments = Some(Seq())
    ))}
    def bindNative1[A1, A2](source : String)(implicit
        metaA1 : Meta[A1],
        metaA2 : Meta[A2]
    ) : A1 => A2 = {a1 : A1 => metaA2.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaA2.glslTypeName,
            argumentTypes = Seq(metaA1.glslTypeName)
        ),
        arguments = Some(Seq(metaA1.untyped(a1)))
    ))}

    def bindNative2[A1, A2, A3](source : String)(implicit
        metaA1 : Meta[A1],
        metaA2 : Meta[A2],
        metaA3 : Meta[A3]
    ) : A1 => A2 => A3 = {a1 : A1 => a2 : A2 => metaA3.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaA3.glslTypeName,
            argumentTypes = Seq(metaA1.glslTypeName, metaA2.glslTypeName)
        ),
        arguments = Some(Seq(metaA1.untyped(a1), metaA2.untyped(a2)))
    ))}

    def bindNative4[A1, A2, A3, A4, A5](source : String)(implicit
        metaA1 : Meta[A1],
        metaA2 : Meta[A2],
        metaA3 : Meta[A3],
        metaA4 : Meta[A4],
        metaA5 : Meta[A5]
    ) : A1 => A2 => A3 => A4 => A5 = {a1 : A1 => a2 : A2 => a3 : A3 => a4 : A4 => metaA5.make(FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            source = source,
            returnType = metaA5.glslTypeName,
            argumentTypes = Seq(metaA1.glslTypeName, metaA2.glslTypeName, metaA3.glslTypeName, metaA4.glslTypeName)
        ),
        arguments = Some(Seq(metaA1.untyped(a1), metaA2.untyped(a2), metaA3.untyped(a3), metaA4.untyped(a4)))
    ))}

}
