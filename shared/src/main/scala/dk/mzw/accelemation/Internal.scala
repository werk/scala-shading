package dk.mzw.accelemation

object Internal {
    abstract sealed class Untyped
    case class Constant(n : Double) extends Untyped
    case class Bind (variableType : String, argument : Untyped, body : Untyped => Untyped) extends Untyped
    case class Variable(n : Int) extends Untyped
    case class Field(label : String, target : Untyped) extends Untyped
    case class If(condition : Untyped, whenTrue : Untyped, whenFalse : Untyped) extends Untyped
    case class BuiltIn(name : String) extends Untyped
    case class Prefix(operator : String,  right: Untyped) extends Untyped
    case class Infix(operator : String,  left: Untyped, right : Untyped) extends Untyped
    case class Call(name : String, arguments : List[Untyped]) extends Untyped
    case class UniformU(ref : Uniform[_], variableType : String) extends Untyped
    case class FunctionDefinitionCall(definition : FunctionDefinition, arguments : Option[Seq[Untyped]]) extends Untyped

    class Uniform[V](
        val name: String,
        var value : V
    )

    case class Signature(
        name : String,
        returnType : String,
        argumentTypes : Seq[String]
    )

    sealed trait FunctionDefinition

    case class DomainFunctionDefinition(
        identity : AnyRef,
        signature : Signature,
        body : Seq[Untyped] => Untyped
    ) extends FunctionDefinition

    case class NativeFunctionDefinition(
        source : String,
        returnType : String,
        argumentTypes : Seq[String]
    ) extends FunctionDefinition
}
