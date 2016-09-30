package dk.mzw.accelemation

import dk.mzw.accelemation.Internal.{Call, _}
import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.HidingDevils

object CompileFunction {

    private def v[A](name : String) : Term[A] = Term(BuiltIn(name))

    case class CompiledFunction(source : String, uniforms : Set[UniformU])

    def function3[A1, A2, A3, A4](f : Term[A1] => Term[A2] => Term[A3] => Term[A4], name : String, a1 : String = "a1", a2 : String = "a2", a3 : String = "a3")(implicit
        typeA1 : VariableType[Term[A1]],
        typeA2 : VariableType[Term[A2]],
        typeA3 : VariableType[Term[A3]],
        typeA4 : VariableType[Term[A4]]
    ) : CompiledFunction = {
        finish(f(v(a1))(v(a2))(v(a3)), name, List(typeA1.t -> a1, typeA2.t -> a2, typeA3.t -> a3))
    }

    def function2[A1, A2, A3](f : Term[A1] => Term[A2] => Term[A3], name : String, a1 : String = "a1", a2 : String = "a2")(implicit
        typeA1 : VariableType[Term[A1]],
        typeA2 : VariableType[Term[A2]],
        typeA3 : VariableType[Term[A3]]
    ) : CompiledFunction = {
        finish(f(v(a1))(v(a2)), name, List(typeA1.t -> a1, typeA2.t -> a2))
    }

    def function1[A1, A2](f : Term[A1] => Term[A2], name : String, a1 : String = "a1")(implicit
        typeA1 : VariableType[Term[A1]],
        typeA2 : VariableType[Term[A2]]
    ) : CompiledFunction = {
        finish(f(Term(BuiltIn(a1))), name, List(typeA1.t -> a1))
    }

    def function0[A](r : Term[A], name : String)(implicit
        typeA1 : VariableType[Term[A]]
    ) : CompiledFunction = {
        finish(r, name, List())
    }


    private def finish[A](r : Term[A], name : String, arguments : List[(String, String)])
         (implicit typeR : VariableType[Term[A]])
        : CompiledFunction =
    {
        val compile = new Compiler()
        val compiled = compile(r.untyped)
        val vs = compile.declarations
        val bindings = vs.reverse.mkString
        val source = s"""
${typeR.t} $name(${arguments.map{case (t, a) => s"$t $a"}.mkString(", ")}) {
$bindings    return $compiled;
}
"""
        CompiledFunction(source, compile.uniforms)

    }

    def main(args: Array[String]) {
        val r : R = 23 + 42
        println(function0(r, "foobar"))

        println(function0(r === r, "foobar"))

        def f(a : R) : Vec2 = vec2(a, a)
        println(function1(f, "foobar", "r"))

        def f2(a : R)(b : R) : Vec3 = vec3(a, a, b)
        println(function2(f2, "foobar", "x", "y"))

        println(function3({x : R => y : R => z : R => vec3(x, y, z)}, "foobar", "x", "y", "z"))

        println(function3(HidingDevils.apply, "hidingDevils", "t", "x", "y"))

    }

    private class Compiler {
        var declarations = List[String]()
        var uniforms = Set[UniformU]()

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
            case uniform : UniformU =>
                uniforms += uniform
                s"${uniform.ref.name}"
        }

    }


}
