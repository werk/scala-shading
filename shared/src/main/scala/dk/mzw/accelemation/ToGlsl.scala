package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language.{Time, R, Term, Animation}

object ToGlsl {

    def apply(f : Animation) : String = {
        val compile = new Compiler()

        val t : Time = Term(BuiltIn("position.w"))
        val x : R = Term(BuiltIn("position.x"))
        val y : R = Term(BuiltIn("position.y"))
        val Term(animation) = f (t) (x) (y)

        val compiled = compile(animation)
        val vs = compile.declarations

        val bindings        =  vs.reverse.mkString
        val before          =
            """
vec4 hsvaToRgba(vec4 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    vec3 r = c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
    return vec4(r, c.a);
}

vec4 animation(vec4 position) {
    float pi = 3.14159265359;
"""
        val after           =
            ";\n}\n"
        before + bindings + "    return " + compiled + after
    }

    private class Compiler {
        var declarations = List[String]()

        def apply(u : Untyped) : String = u match {
            case Constant(n) =>
                // We need the decimal point to denote a float.
                // Scala JS will not generate this for integers.
                // This is not a problem in JVM
                val s = n.toString
                if(s.contains('.')) s
                else s + ".0";
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
            case Call(name, arguments) => s"$name(${arguments.map(apply).mkString(", ")})"
        }

    }
}
