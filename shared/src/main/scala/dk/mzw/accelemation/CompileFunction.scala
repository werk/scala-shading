package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language._

object CompileFunction {

    private val formalParameters = Stream.iterate(1)({_ + 1}).map("a" + _)

    private case class CompiledFunction(name : String, source : String, uniforms : Set[UniformU], dependencies : Set[CompiledFunction])
    case class SourceAndUniforms(source : String, uniforms : Set[UniformU])

    def compileAnimationWithDependencies(f : Animation, name : String, t : String, x : String, y : String) : SourceAndUniforms = {
        val bound = bind3(f, name)
        val applied = bound (Term[Double](BuiltIn(t))) (Term[Double](BuiltIn(x))) (Term[Double](BuiltIn(y)))
        val definition = applied.untyped.asInstanceOf[FunctionDefinitionCall].definition
        val compiler = new FunctionDefinitionCompiler
        val root = compiler(definition)
        val graph = Topological.toGraph(root.name, compiler.getByName(_).dependencies.map(_.name))
        val sortedNames = Topological(graph)
        val sortedCompiled = sortedNames.map(compiler.getByName)
        val uniforms = sortedCompiled.flatMap(_.uniforms).toSet
        SourceAndUniforms(sortedCompiled.map(_.source).mkString("\n"), uniforms)
    }

    private class FunctionDefinitionCompiler {
        private type FunctionSource = String
        private var usedNames = Set[String]()
        private var compiled = Map[FunctionDefinition, FunctionSource]()
        private var canonical = Map[FunctionSource, FunctionDefinition]()

        private def unusedName(nameHint : String) : String = {
            val names = nameHint +: Stream.iterate(1)({_ + 1}).map(nameHint + _)
            val chosen = names.dropWhile(usedNames).head
            usedNames += chosen
            chosen
        }

        def apply(root : FunctionDefinition) : CompiledFunction = {
            null
        }

        def getByName(name : String) : CompiledFunction = {
            null
        }

        def compileFunctionDefinition(definition : FunctionDefinition) : CompiledFunction = {
            val argumentsNames = formalParameters.take(definition.signature.argumentTypes.length).toList
            val body = definition.body(argumentsNames.map(BuiltIn))

            val compile = new Compiler()
            val compiled = compile(body)
            val lines = (s"return $compiled" :: compile.declarations).reverse.mkString
            val source = formatFunction(definition.signature, argumentsNames, lines)
            val dependencies = compile.functions.map(compileFunctionDefinition)
            CompiledFunction(source, compile.uniforms, dependencies)
        }

        def formatFunction(signature : Signature, argumentsNames : List[String], body : String) = {
            val typedParameters = signature.argumentTypes.zip(argumentsNames).map{case (t, v) => s"$t $v"}
            s"""
${signature.returnType} ${signature.name}(${typedParameters.mkString(", ")}) {
$body    return $compiled;
}"""
        }
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



    // Experiment copied, fix or delete

    case class Compiled()
    case class CompiledExpression(
        declarations : List[String],
        uniforms : Set[UniformU],
        functions : Set[FunctionDefinition]
    )

    def compile(root : FunctionDefinition) : Compiled = {
        type FunctionSource = String
        var usedNames = Set[String]()
        var compiled = Map[FunctionDefinition, CompiledFunction]()
        var canonical = Map[FunctionSource, FunctionDefinition]()

        def unusedName(nameHint : String) : String = {
            val names = nameHint +: Stream.iterate(1)({_ + 1}).map(nameHint + _)
            val chosen = names.dropWhile(usedNames).head
            usedNames += chosen
            chosen
        }

        def compileFunction(definition : FunctionDefinition): String = {
            val argumentsNames = formalParameters.take(definition.signature.argumentTypes.length).toList
            val body = definition.body(argumentsNames.map(BuiltIn))

            val compile = new Compiler()
            val compiled = compile(body)
            val lines = (s"return $compiled" :: compile.declarations).reverse.mkString
            val name = unusedName(definition.signature.name)
            val source = formatFunction(definition.signature.copy(name = name), argumentsNames, lines)
            //val dependencies = compile.functions.map(compileFunctionDefinition)
            //CompiledFunction(source, compile.uniforms, dependencies)
            name
        }

        def formatFunction(signature : Signature, argumentsNames : List[String], body : String) = {
            val typedParameters = signature.argumentTypes.zip(argumentsNames).map{case (t, v) => s"$t $v"}
            s"""
${signature.returnType} ${signature.name}(${typedParameters.mkString(", ")}) {
$body    return $compiled;
}"""
        }

        class Compiler {
            var declarations = List[String]()
            var uniforms = Set[UniformU]()
            var functions = Set[String]()

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
                    val name = compileFunction(definition)
                    functions += name
                    s"$name(${call.map(apply).mkString(", ")})"
            }
        }

        compileFunction(root)
    }

}
