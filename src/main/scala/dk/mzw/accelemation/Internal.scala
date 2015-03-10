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
    /*case class LiftVec2(name : String, infix : Boolean, arguments : List[Untyped]) extends Untyped
    case class LiftVec3(name : String, infix : Boolean, arguments : List[Untyped]) extends Untyped
    case class LiftVec4(name : String, infix : Boolean, arguments : List[Untyped]) extends Untyped*/
}
