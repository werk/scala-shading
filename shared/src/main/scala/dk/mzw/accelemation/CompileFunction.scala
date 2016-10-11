package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language._

object CompileFunction {

    private val formalParameters = Stream.iterate(1)({_ + 1}).map("a" + _)

    case class CompiledFunction(source : String, uniforms : Set[UniformU], dependencies : Set[FunctionDefinition])
    case class SourceAndUniforms(source : String, uniforms : Set[UniformU])

    def compileAnimationWithDependencies(f : Animation, name : String, t : String, x : String, y : String) : SourceAndUniforms = {
        val bound = bind3(f, name)
        val applied = bound (Term[Double](BuiltIn(t))) (Term[Double](BuiltIn(x))) (Term[Double](BuiltIn(y)))
        val definition = applied.untyped.asInstanceOf[FunctionDefinitionCall].definition
        val graph = Topological.toGraph(definition, {b : FunctionDefinition => compileFunctionDefinition(b).dependencies}) // Note this will compile and discard the source code
        val sorted = Topological(graph)
        val functionSources = sorted.map(compileFunctionDefinition(_).source)
        val uniforms = Set[UniformU]()
        SourceAndUniforms(functionSources.mkString("\n"), uniforms)
    }

    def compileFunctionDefinition(definition : FunctionDefinition) : CompiledFunction = {
        val argumentsNames = formalParameters.take(definition.signature.argumentTypes.length)
        val body = definition.body(argumentsNames.map(BuiltIn))

        val compile = new Compiler()
        val compiled = compile(body)
        val vs = compile.declarations
        val bindings = vs.reverse.mkString
        val typedParameters = definition.signature.argumentTypes.zip(argumentsNames).map{case (t, v) => s"$t $v"}
        val source = s"""
${definition.signature.returnType} ${definition.signature.name}(${typedParameters.mkString(", ")}) {
$bindings    return $compiled;
}"""
        CompiledFunction(source, compile.uniforms, compile.functions)
    }

    private class Compiler {
        var declarations = List[String]()
        var uniforms = Set[UniformU]()
        var functions = Set[FunctionDefinition]()

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
            case FunctionDefinitionCall(definition, call) =>
                functions += definition
                s"${definition.signature.name}(${call.map(apply).mkString(", ")})"
        }
    }
}
