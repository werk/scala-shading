package dk.mzw.scalashading

import dk.mzw.scalashading.Language._
import dk.mzw.scalashading.Math._
import dk.mzw.scalashading.internal.FunctionParser
import dk.mzw.scalashading.internal.FunctionParser._
import dk.mzw.scalashading.internal.Internal._
import dk.mzw.scalashading.internal.Topological


object Compile {
    case class SourceAndUniforms(source : String, uniforms : Set[UniformU])

    def pixelToUnit(resolution : Vec2)(pixel : Vec2) : Vec2 = {
        val streched_position = (pixel / resolution) * 2.0 - 1.0
        val aspect = Vec2(max(resolution.x / resolution.y, 1.0), max(resolution.y / resolution.x, 1.0))
        streched_position * aspect
    }

    val u_timeUniform = new Uniform[Double]("u_time", 0)
    val u_resolutionUniform = new Uniform[(Double, Double)]("u_resolution", (1,1))
    val unitCoordinateTransform = pixelToUnit(u_resolutionUniform) _

    def apply(
        f : Animation,
        coordinateTransform : Vec2 => Vec2 = unitCoordinateTransform,
        timeUniform : Uniform[Double] = u_timeUniform
    ) : String = {
        val g : Animation = {t => x => y =>
            coordinateTransform(Vec2(x, y)) bind {xy =>
                f(t) (xy.x) (xy.y)
            }
        }
        pixel(g (timeUniform))
    }

    def pixel(f : Image) : String = {
        val pixel = Vec2(BuiltIn("gl_FragCoord"))
        val applied = f (pixel.x) (pixel.y)
        val SourceAndUniforms(source, _) = new GlobalScope().compile(untyped(applied))
        source
    }

    def image(f : Image, coordinates : Vec2) : String = {
        val applied = f (coordinates.x) (coordinates.y)
        val SourceAndUniforms(source, _) = new GlobalScope().compile(untyped(applied))
        source
    }

    class GlobalScope {

        def compile(expression : Untyped) : SourceAndUniforms = {
            val compiled = compileExpression(expression)
            val sorted = sortedDependencies(compiled.dependencies)
            val uniforms = sorted.flatMap(_.uniforms).toSet ++ compiled.uniforms

            val us = uniforms.map(u => s"uniform ${u.variableType} ${u.ref.name};").mkString("\n")
            val vs = "varying vec2 v_textureCoordinates;" // TODO
            val functions = sorted.map(_.source).mkString("\n")
            val source =
s"""precision highp float;
$us
$vs

$functions
void main() {
    ${compiled.declarations.mkString("\n    ")}
    gl_FragColor = ${compiled.expressionCode};
}"""

            SourceAndUniforms(source, uniforms)
        }

        def compile(function : FunctionDefinition) : SourceAndUniforms = {
            val c = compileFunction(function)
            val sorted = sortedDependencies(c)
            val uniforms = sorted.flatMap(_.uniforms).toSet
            SourceAndUniforms(sorted.map(_.source).mkString("\n"), uniforms)
        }

        private def sortedDependencies(root : CompiledFunction) : List[CompiledFunction] = {
            val graph = Topological.toGraph(root, {d : CompiledFunction => d.dependencies})
            Topological(graph)
        }

        private def sortedDependencies(roots : Set[CompiledFunction]) : List[CompiledFunction] = {
            val fakeRoot = CompiledFunction(null, null, null, roots)
            val sorted = sortedDependencies(fakeRoot)
            sorted.init
        }

        private val formalParameters = Stream.iterate(1)({_ + 1}).map("a" + _)
        private var usedNames = Set[String]()
        private var canonical = Map[NamelessEqualityBase, CompiledFunction]()

        private case class NamelessEqualityBase(
            returnType : String,
            argumentTypes : Seq[String],
            body : List[String]
        )

        private case class CompiledFunction(
            name : String,
            source : String,
            uniforms : Set[UniformU],
            dependencies : Set[CompiledFunction]
        )

        private case class CompileExpression(
            expressionCode : String,
            declarations : List[String],
            uniforms : Set[UniformU],
            dependencies : Set[CompiledFunction]
        )

        private def unusedName(nameHint : String) : String = {
            val names = nameHint +: Stream.iterate(1)({_ + 1}).map(nameHint + _)
            val chosen = names.dropWhile(usedNames).head
            usedNames += chosen
            chosen
        }

        private def compileFunction(definition : FunctionDefinition) : CompiledFunction = definition match {
            case d : DomainFunctionDefinition => compileDomainFunction(d)
            case d : NativeFunctionDefinition => compileNativeFunction(d)
        }

        private def compileDomainFunction(definition : DomainFunctionDefinition) : CompiledFunction = {
            val argumentsNames = formalParameters.take(definition.signature.argumentTypes.length).toList
            val body = definition.body(argumentsNames.map(BuiltIn))

            val compiled = compileExpression(body)
            val lines =  compiled.declarations ++ List(s"return ${compiled.expressionCode};")
            val base = NamelessEqualityBase(definition.signature.returnType, definition.signature.argumentTypes, lines)
            canonical.get(base) match {
                case Some(cached) => cached
                case None =>
                    val name = unusedName(definition.signature.name)
                    val source = formatFunction(definition.signature.copy(name = name), argumentsNames, lines)
                    val compiledFunction = CompiledFunction(name, source, compiled.uniforms, compiled.dependencies)
                    canonical += base -> compiledFunction
                    compiledFunction
            }
        }

        private def compileNativeFunction(definition : NativeFunctionDefinition) : CompiledFunction = {
            val definitions = FunctionParser.parse(definition.source)

            var previous : Option[CompiledFunction] = None
            definitions.map{d =>
                val compiledFunction = d match {
                    case f: DecomposedFunction =>
                        val base = NamelessEqualityBase(f.returnType, f.typedArguments.map(_._1), f.body)
                        canonical.get(base) match {
                            case Some(cached) => cached
                            case None =>
                                val name = unusedName(f.name)
                                val source = FunctionParser.reassemble(f.copy(name = name))
                                val c = CompiledFunction(name, source, Set(), previous.toSet)
                                canonical += base -> c
                                c
                        }
                    case c: DecomposedConstant =>
                        val base = NamelessEqualityBase(c.returnType, Seq(), List(c.value))
                        canonical.get(base) match {
                            case Some(cached) => cached
                            case None =>
                                val name = unusedName(c.name)
                                val source = FunctionParser.reassemble(c.copy(name = name))
                                val f = CompiledFunction(name, source, Set(), previous.toSet)
                                canonical += base -> f
                                f
                        }
                }
                previous = Some(compiledFunction)
                compiledFunction
            }
            previous.get
        }

        private def formatFunction(signature : Signature, argumentsNames : List[String], body : List[String]) = {
            val typedParameters = signature.argumentTypes.zip(argumentsNames).map{case (t, v) => s"$t $v"}
            s"${signature.returnType} ${signature.name}(${typedParameters.mkString(", ")}) {\n${body.map("    " + _ + "\n").mkString}}\n"
        }

        private def compileExpression(expression : Untyped) : CompileExpression = {
            var declarations = List[String]()
            var uniforms = Set[UniformU]()
            var functions = Set[CompiledFunction]()

            def inner(u : Untyped) : String = u match {
                case ConstantFloat(n) =>
                    // We need the decimal point to denote a float.
                    // Scala JS will not generate this for integers.
                    // This is not a problem in JVM
                    val s = n.toString
                    if(s.contains('.')) s
                    else s + ".0";
                case ConstantInt(i) => i.toString
                case ConstantBoolean(b) => b.toString
                case Bind(variableType, argument, body) =>
                    val a = inner(argument)
                    val i = declarations.length
                    val declaration = s"$variableType v$i = $a;"
                    declarations ::= declaration
                    inner(body(Variable(i)))
                case Variable(n) => s"v$n"
                case Field(label, target) => s"${inner(target)}.$label"
                case If(condition, whenTrue, whenFalse) => s"(${inner(condition)} ? ${inner(whenTrue)} : ${inner(whenFalse)})"
                case BuiltIn(name) => name
                case Prefix(operator, right) => s"($operator${inner(right)})"
                case Infix(operator, left, right) => s"(${inner(left)} $operator ${inner(right)})"
                case Call(name, arguments) => s"$name(${arguments.map(inner).mkString(", ")})"
                case uniform : UniformU =>
                    uniforms += uniform
                    s"${uniform.ref.name}"
                case FunctionDefinitionCall(definition, arguments) =>
                    val compiledFunction = compileFunction(definition)
                    functions += compiledFunction
                    val call = arguments.map{as => "(" + as.map(inner).mkString(", ") + ")"}.getOrElse("")
                    s"${compiledFunction.name}$call"
            }

            CompileExpression(
                expressionCode = inner(expression),
                declarations = declarations.reverse,
                uniforms = uniforms,
                dependencies = functions
            )
        }

    }
}
