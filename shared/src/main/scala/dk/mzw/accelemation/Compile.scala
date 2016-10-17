package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language._

object Compile {
    case class SourceAndUniforms(source : String, uniforms : Set[UniformU])

    def apply(f : Animation, name : String, t : String, x : String, y : String) : SourceAndUniforms = {
        val bound = bind3(f, name)
        val applied = bound (Term[Double](BuiltIn(t))) (Term[Double](BuiltIn(x))) (Term[Double](BuiltIn(y)))
        val definition = applied.untyped.asInstanceOf[FunctionDefinitionCall].definition
        compile(definition)
    }

    private def compile(root : FunctionDefinition) : SourceAndUniforms = {
        val formalParameters = Stream.iterate(1)({_ + 1}).map("a" + _)
        var usedNames = Set[String]()
        var canonical = Map[NamelessEqualityBase, CompiledFunction]()

        case class NamelessEqualityBase(returnType : String, argumentTypes : Seq[String], body : List[String])
        case class CompiledFunction(name : String, source : String, uniforms : Set[UniformU], dependencies : Set[CompiledFunction])

        def unusedName(nameHint : String) : String = {
            val names = nameHint +: Stream.iterate(1)({_ + 1}).map(nameHint + _)
            val chosen = names.dropWhile(usedNames).head
            usedNames += chosen
            chosen
        }

        def compileFunction(definition : FunctionDefinition) : CompiledFunction = definition match {
            case d : NativeFunctionDefinition => compileNativeFunction(d)
            case d : ForeignFunctionDefinition => compileForeignFunction(d)
        }

        def compileNativeFunction(definition : NativeFunctionDefinition) : CompiledFunction = {
            val argumentsNames = formalParameters.take(definition.signature.argumentTypes.length).toList
            val body = definition.body(argumentsNames.map(BuiltIn))

            val compile = new CompileExpression()
            val compiled = unparenthesize(compile(body))
            val lines = (s"return $compiled;" :: compile.declarations).reverse
            val base = NamelessEqualityBase(definition.signature.returnType, definition.signature.argumentTypes, lines)
            canonical.get(base) match {
                case Some(cached) => cached
                case None =>
                    val name = unusedName(definition.signature.name)
                    val source = formatFunction(definition.signature.copy(name = name), argumentsNames, lines)
                    val compiledFunction = CompiledFunction(name, source, compile.uniforms, compile.functions)
                    canonical += base -> compiledFunction
                    compiledFunction
            }
        }

        def compileForeignFunction(definition : ForeignFunctionDefinition) : CompiledFunction = {
            val parser = new GlslFunctionParser(definition.source)
            val base = NamelessEqualityBase(parser.returnType, parser.argumentTypes, parser.bodyLines)
            canonical.get(base) match {
                case Some(cached) => cached
                case None =>
                    val name = unusedName(parser.name)
                    CompiledFunction(name, parser.reassemble(name), Set(), Set())
            }
        }

        def formatFunction(signature : Signature, argumentsNames : List[String], body : List[String]) = {
            val typedParameters = signature.argumentTypes.zip(argumentsNames).map{case (t, v) => s"$t $v"}
            s"${signature.returnType} ${signature.name}(${typedParameters.mkString(", ")}) {\n${body.map("    " + _ + "\n").mkString}}\n"
        }

        class CompileExpression {
            var declarations = List[String]()
            var uniforms = Set[UniformU]()
            var functions = Set[CompiledFunction]()

            def apply(u : Untyped) : String = u match {
                case Constant(n) =>
                    // We need the decimal point to denote a float.
                    // Scala JS will not generate this for integers.
                    // This is not a problem in JVM
                    val s = n.toString
                    if(s.contains('.')) s
                    else s + ".0";
                case Bind(variableType, argument, body) =>
                    val a = unparenthesize(apply(argument))
                    val i = declarations.length
                    val declaration = s"$variableType v$i = $a;"
                    declarations ::= declaration
                    apply(body(Variable(i)))
                case Variable(n) => s"v$n"
                case Field(label, target) => s"${apply(target)}.$label"
                case If(condition, whenTrue, whenFalse) => s"(${apply(condition)} ? ${apply(whenTrue)} : ${apply(whenFalse)})"
                case BuiltIn(name) => name
                case Prefix(operator, right) => s"($operator${apply(right)})"
                case Infix(operator, left, right) => s"(${apply(left)} $operator ${apply(right)})"
                case Call(name, arguments) => s"$name(${arguments.map((apply _).andThen(unparenthesize)).mkString(", ")})"
                case uniform : UniformU =>
                    uniforms += uniform
                    s"${uniform.ref.name}"
                case FunctionDefinitionCall(definition, call) =>
                    val compiledFunction = compileFunction(definition)
                    functions += compiledFunction
                    s"${compiledFunction.name}(${call.map((apply _).andThen(unparenthesize)).mkString(", ")})"
            }
        }

        val c = compileFunction(root)
        val graph = Topological.toGraph(c, {d : CompiledFunction => d.dependencies})
        val sorted = Topological(graph)
        val uniforms = sorted.flatMap(_.uniforms).toSet
        SourceAndUniforms(sorted.map(_.source).mkString("\n"), uniforms)

    }

    def unparenthesize(s : String) : String = {
        if (s.startsWith("(") && s.endsWith(")")) unparenthesize(s.drop(1).dropRight(1))
        else s
    }
}

class GlslFunctionParser(glsl : String) {
    private val in = new MutableString(glsl.trim)
    val returnType = in.takeWhile(_.isLetterOrDigit).mkString
    in.dropWhile(_.isWhitespace)
    val name = in.takeWhile(_.isLetterOrDigit).mkString
    in.dropWhile(c => c.isWhitespace || c == '(')
    private val parameters = in.takeWhile(_ != ')').mkString
    val arguments = parameters.trim.split("""\s*,\s*""").map(_.split("""\s+""") match {case Array(t, v) => (t, v)}).toList
    in.dropWhile(c => c == ')' || c.isWhitespace || c == '{')
    private val body = in.takeWhile(_ != '}').mkString
    var bodyLines = body.trim.split("""\s*[\n\r]+\s*""").toList

    def argumentTypes = arguments.map(_._1)

    def reassemble(rename : String) =
s"""
$returnType $rename(${arguments.map{case (t, v) => s"$t $v"}.mkString(", ")}) {
${bodyLines.map("    " + _).mkString("\n")}
}
"""
}

class MutableString(var s : String) {
    def takeWhile(p : Char => Boolean) : String = {
        val part = s.takeWhile(p)
        s = s.drop(part.length)
        part
    }

    def dropWhile(p : Char => Boolean) : Unit = takeWhile(p)

    override def toString() = s
}

