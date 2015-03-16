package dk.mzw.accelemation

import Internal._

object Language {

    case class Term[A](untyped : Untyped)

    type R = Term[Double]
    type Vec2 = Term[(Double, Double)]
    type Vec3 = Term[(Double, Double, Double)]
    type Vec4 = Term[(Double, Double, Double, Double)]

    type B = Term[Boolean]

    type Time = R
    type Color = Vec4

    type Image = R => R => Color
    type Animation = Time => Image

    def vec2(x : R, y : R) : Vec2 = Term(Call("vec2",List(x.untyped, y.untyped)))
    def vec3(x : R, y : R, z : R) : Vec3 = Term(Call("vec3",List(x.untyped, y.untyped, z.untyped)))
    def vec4(x : R, y : R, z : R, u : R) : Vec4 = Term(Call("vec4",List(x.untyped, y.untyped, z.untyped, u.untyped)))

    def if_[A](condition : B, whenTrue : Term[A], whenFalse : Term[A]) : Term[A] = Term(If(condition.untyped, whenTrue.untyped, whenFalse.untyped))

    val rgba : (R, R, R, R) => Color = vec4
    def hsva (h : R, s : R, v : R, a : R) : Color = Term(Call("hsvaToRgba",List(Call("vec4",List(h.untyped, s.untyped, v.untyped, a.untyped)))))

    implicit def fromDouble(r : Double) : R = Term(Constant(r))
    implicit def fromInteger(r : Int) : R = fromDouble(r.toDouble)

    implicit class RealWithOperations[A](a : A) (implicit toReal : A => R){
        def +(b : R) : R = Term(Infix("+", a.untyped, b.untyped))
        def -(b : R) : R = Term(Infix("-", a.untyped, b.untyped))
        def *(b : R) : R = Term(Infix("*", a.untyped, b.untyped))
        def unary_-() : R = Term(Prefix("-", a.untyped))
        def /(b : R) : R = Term(Infix("/", a.untyped, b.untyped))

        def ===(b : R) : B = Term(Infix("==", a.untyped, b.untyped))
        def !=(b : R) : B = Term(Infix("!=", a.untyped, b.untyped))
        def <(b : R) : B = Term(Infix("<", a.untyped, b.untyped))
        def >(b : R) : B = Term(Infix(">", a.untyped, b.untyped))
        def <=(b : R) : B = Term(Infix("<=", a.untyped, b.untyped))
        def >=(b : R) : B = Term(Infix(">=", a.untyped, b.untyped))
    }

    case object IsVector
    implicit def vec2IsVector(v : Vec2) : IsVector.type = IsVector
    implicit def vec3IsVector(v : Vec3) : IsVector.type = IsVector
    implicit def vec4IsVector(v : Vec4) : IsVector.type = IsVector

    implicit class VectorWithOperations[T](a : Term[T])(implicit evidence: Term[T] => IsVector.type){
        def dot(b : Term[T]) : R = Term(Call("dot", List(a.untyped, b.untyped)))
        def magnitude : R = Term(Call("length", List(a.untyped)))
        def distance(b : Term[T]) : R = Term(Call("distance", List(a.untyped, b.untyped)))
        def normalize : Term[T] = Term(Call("normalize", List(a.untyped)))
        def faceforward(b : Term[T], c : Term[T]) : Term[T] = Term(Call("faceforward", List(a.untyped, b.untyped, c.untyped)))
        def reflect(b : Term[T]) : Term[T] = Term(Call("reflect", List(a.untyped, b.untyped)))
    }

    object Math {
        val pi : R = Term(BuiltIn("pi"))

        def abs(a : R) : R = Term(Call("abs", List(a.untyped)))
        def sign(a : R) : R = Term(Call("sign", List(a.untyped)))
        def sqrt(a : R) : R = Term(Call("sqrt", List(a.untyped)))
        def exp (a : R) : R = Term(Call("exp", List(a.untyped)))
        def log (a : R) : R = Term(Call("log", List(a.untyped)))
        def sin (a : R) : R = Term(Call("sin", List(a.untyped)))
        def tan (a : R) : R = Term(Call("tan", List(a.untyped)))
        def cos (a : R) : R = Term(Call("cos", List(a.untyped)))
        def asin (a : R) : R = Term(Call("asin", List(a.untyped)))
        def atan (a : R) : R = Term(Call("atan", List(a.untyped)))
        def acos (a : R) : R = Term(Call("acos", List(a.untyped)))
        def sinh (a : R) : R = Term(Call("sinh", List(a.untyped)))
        def tanh (a : R) : R = Term(Call("tanh", List(a.untyped)))
        def cosh (a : R) : R = Term(Call("cosh", List(a.untyped)))
        def asinh (a : R) : R = Term(Call("asinh", List(a.untyped)))
        def atanh (a : R) : R = Term(Call("atanh", List(a.untyped)))
        def acosh (a : R) : R = Term(Call("acosh", List(a.untyped)))

        def pow(a : R, b : R) : R = Term(Call("pow", List(a.untyped, b.untyped)))
        def max (a : R, b : R) : R = Term(Call("max", List(a.untyped, b.untyped)))
        def min (a : R, b : R) : R = Term(Call("min", List(a.untyped, b.untyped)))
        def mod (a : R, b : R) : R = Term(Call("mod", List(a.untyped, b.untyped)))
        def round (a : R) : R = Term(Call("round", List(a.untyped)))
        def floor (a : R) : R = Term(Call("floor", List(a.untyped)))
        def ceil (a : R) : R = Term(Call("ceil", List(a.untyped)))
    }

    implicit class BoolWithOperations(a : B){
        def unary_!() : B = Term(Prefix("!", a.untyped))
        def &&(b : B) : B = Term(Infix("&&", a.untyped, b.untyped))
        def ||(b : B) : B = Term(Infix("||", a.untyped, b.untyped))
    }

    implicit class ColorWithOperations(a : Color){
        def red : R = Term(Field("x", a.untyped))
        def green : R = Term(Field("y", a.untyped))
        def blue : R = Term(Field("z", a.untyped))
        def alpha : R = Term(Field("w", a.untyped))
    }

    case class VariableType(t : String)
    implicit def BoolType(v : B) : VariableType = VariableType("bool")
    implicit def RealType(v : R) : VariableType = VariableType("float")
    implicit def Vec2Type(v : Vec2) : VariableType = VariableType("vec2")
    implicit def Vec3Type(v : Vec3) : VariableType = VariableType("vec3")
    implicit def Vec4Type(v : Vec4) : VariableType = VariableType("vec4")

    implicit class Bindable[A](a : Term[A]) (implicit variableType : Term[A] => VariableType){
        def bind[B](f : Term[A] => Term[B]) : Term[B] = {
            def body(a: Untyped): Untyped = f(Term(a)).untyped
            Term(Bind(variableType(a).t, a.untyped, body))
        }
    }

}
