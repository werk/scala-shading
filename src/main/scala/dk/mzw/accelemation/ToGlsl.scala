package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language.{Time, R, Term, Animation}

object ToGlsl {

    def apply(f : Animation) : String = {
        val compile = new Compiler()

        val t : Time = Term(BuiltIn("t"))
        val x : R = Term(BuiltIn("x"))
        val y : R = Term(BuiltIn("y"))
        val Term(animation) = f (t) (x) (y)

        val compiled = compile(animation)
        val vs = compile.declarations

        val bindings        =  vs.reverse.mkString
        val before          =
            "uniform float u_time;\n" ++
            "uniform float u_aspectRatio;\n" ++
            "uniform vec2  u_resolution;\n" ++
            "vec4 hsvaToRgba(vec4 c) {\n" ++
            "    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);\n" ++
            "    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);\n" ++
            "    vec3 r = c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);\n" ++
            "    return vec4(r.x, r.y, r.z, c.w);\n" ++
            "}\n" ++
            "void main() {\n" ++
            "    float pi = 3.14159265359;\n" ++
            "    float t = u_time;\n" ++
            "    float mystery = 1.23;\n" ++
            "    float x = (gl_FragCoord.x / u_resolution.x) * 2.0 * u_aspectRatio - u_aspectRatio * mystery;\n" ++
            "    float y = (gl_FragCoord.y / u_resolution.y) * 2.0 - 1.0 * mystery;\n"
        val after           =
            ";\n}\n"
        before + bindings + "    gl_FragColor = " + compiled + after
    }

    private class Compiler {
        var declarations = List[String]()

        def apply(u : Untyped) : String = u match {
            case Constant(n) => n.toString
            case Bind(variableType, argument, body) =>
                val a = apply(argument)
                val i = declarations.length
                val declaration = s"    $variableType v$i = $a;\n"
                declarations ::= declaration
                apply(body(Variable(i)))
            case Variable(n) => s"v$n"
            case Field(label, target) => s"${apply(target)}.$label"
            case If(condition, whenTrue, whenFalse) => s"(${apply(condition)} ? ${apply(whenTrue)} : ${apply(whenFalse)})"
            case BuiltIn(name) => name
            case Prefix(operator, right) => s"($operator${apply(right)})"
            case Infix(operator, left, right) => s"(${apply(left)} $operator ${apply(right)})"
            case Call(name, arguments) => s"$name(${arguments.map(apply).mkString(",")})"
        }

    }
}
