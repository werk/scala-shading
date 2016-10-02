package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.HidingDevils

object CompileFunction {

    // Move to language
    case class Fun[F](typeNames : List[String])
    implicit def functionPrimitive[P](implicit a : VariableType[P]) = Fun[P](List(a.t))
    implicit def functionArrow[A, F](implicit a : VariableType[A], f : Fun[F]) = Fun[A => F](a.t :: f.typeNames)

    case class CompiledFunction(source : String, uniforms : Set[UniformU], dependencies : Set[BindF3])
    case class SourceAndUniforms(source : String, uniforms : Set[UniformU])

    def compileAnimationWithDependencies(f : Animation) : List[String] = {
        val bound = bind3("animation", f).untyped.asInstanceOf[BindF3]
        val graph = Topological.toGraph(bound, {b : BindF3 => compileBound(b).dependencies}) // Note this will compile and discard the source code
        val sorted = Topological(graph)
        sorted.map(compileBound(_).source)
    }

    def compileBound(bound : BindF3) : CompiledFunction = {
        val List(a1, a2, a3) = List("a1", "a2", "a3")
        val body = bound.f (BuiltIn(a1)) (BuiltIn(a2)) (BuiltIn(a3))
        finish(body, bound.t4, bound.name, List(bound.t1 -> a1, bound.t2 -> a2, bound.t3 -> a3))
    }


    // Move to language
    def bind3(name : String, f : R => R => R => Vec4)(implicit a : Fun[R => R => R => Vec4]) : Term[R => R => R => Vec4] = {
        val List(t1, t2, t3, t4) = a.typeNames
        def fu(a1 : Untyped) (a2 : Untyped) (a3 : Untyped) : Untyped = f(Term(a1)) (Term(a2)) (Term(a3)).untyped
        Term(BindF3(name, fu, t1, t2, t3, t4))
    }

    // Move to language
    implicit class F3WithOperations(a : Term[R => R => R => Vec4]){
        def apply(a1 : R, a2 : R, a3 : R) : Vec4 = Term(CallF3(a.untyped, a1.untyped, a2.untyped, a3.untyped))
    }


    private def v[A](name : String) : Term[A] = Term(BuiltIn(name))


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
        : CompiledFunction = finish(r.untyped, typeR.t, name, arguments)

    private def finish(body : Untyped, returnType : String, name : String, arguments : List[(String, String)]) : CompiledFunction = {
        val compile = new Compiler()
        val compiled = compile(body)
        val vs = compile.declarations
        val bindings = vs.reverse.mkString
        val source = s"""
$returnType $name(${arguments.map{case (t, a) => s"$t $a"}.mkString(", ")}) {
$bindings    return $compiled;
}"""
        CompiledFunction(source, compile.uniforms, compile.functions)

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
        var functions = Set[BindF3]()

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
            case CallF3(bound : BindF3, a1, a2, a3) =>
                functions += bound
                s"${bound.name}(${List(a1, a2, a3).map(apply).mkString(", ")})"
        }

    }


}
