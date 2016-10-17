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
    def rgbaToHsva (rgbaColor : Color) : Color = Term(Call("rgbaToHsva",List(rgbaColor.untyped)))
    def simplexNoise (x : R, y : R, z : R) : R = Term(Call("snoise",List(Call("vec3",List(x.untyped, y.untyped, z.untyped)))))

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
        def round (a : R) : R = floor(0.5 + a)
        def floor (a : R) : R = Term(Call("floor", List(a.untyped)))
        def ceil (a : R) : R = Term(Call("ceil", List(a.untyped)))
        def atan2(x : R, y : R) : R = 2 * atan(y / (sqrt (pow(x, 2) + pow(y, 2)) + x))
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

    case class VariableType[T](t : String)
    implicit def BoolType = VariableType[B]("bool")
    implicit def RealType = VariableType[R]("float")
    implicit def Vec2Type = VariableType[Vec2]("vec2")
    implicit def Vec3Type = VariableType[Vec3]("vec3")
    implicit def Vec4Type = VariableType[Vec4]("vec4")

    implicit class Bindable[A](a : Term[A]) (implicit variableType : VariableType[Term[A]]){
        def bind[B](f : Term[A] => Term[B]) : Term[B] = {
            def body(a: Untyped): Untyped = f(Term(a)).untyped
            Term(Bind(variableType.t, a.untyped, body))
        }
    }

    implicit def liftUniformB(uniform : Uniform[Boolean]) : B = Term(UniformU(uniform, "bool"))
    implicit def liftUniformR(uniform : Uniform[Double]) : R = Term(UniformU(uniform, "float"))
    implicit def liftUniformVec2(uniform : Uniform[(Double, Double)]) : Vec2 = Term(UniformU(uniform, "vec2"))
    implicit def liftUniformVec3(uniform : Uniform[(Double, Double, Double)]) : Vec3 = Term(UniformU(uniform, "vec3"))
    implicit def liftUniformVec4(uniform : Uniform[(Double, Double, Double, Double)]) : Vec4 = Term(UniformU(uniform, "vec4"))


    // Bind native functions

    def bind1[A1, A2](f : Term[A1] => Term[A2], nameHint : String)(implicit
        typeA1 : VariableType[Term[A1]],
        typeA2 : VariableType[Term[A2]]
    ) : Term[A1] => Term[A2] = {a1 : Term[A1] => Term[A2](FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            identity = f,
            signature = Signature(nameHint, typeA2.t, Seq(typeA1.t)),
            body = {case Seq(a) => f(Term[A1](a)).untyped}
        ),
        arguments = Seq(a1.untyped)
    ))}

    def bind2[A1, A2, A3](f : Term[A1] => Term[A2] => Term[A3], nameHint : String)(implicit
        typeA1 : VariableType[Term[A1]],
        typeA2 : VariableType[Term[A2]],
        typeA3 : VariableType[Term[A3]]
    ) : Term[A1] => Term[A2] => Term[A3] = {a1 : Term[A1] => a2 : Term[A2] => Term[A3](FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            identity = f,
            signature = Signature(nameHint, typeA3.t, Seq(typeA1.t, typeA2.t)),
            body = {case Seq(a, b) => f(Term[A1](a))(Term[A2](b)).untyped}
        ),
        arguments = Seq(a1.untyped, a2.untyped)
    ))}

    def bind3[A1, A2, A3, A4](f : Term[A1] => Term[A2] => Term[A3] => Term[A4], nameHint : String)(implicit
        typeA1 : VariableType[Term[A1]],
        typeA2 : VariableType[Term[A2]],
        typeA3 : VariableType[Term[A3]],
        typeA4 : VariableType[Term[A4]]
    ) : Term[A1] => Term[A2] => Term[A3] => Term[A4] = {a1 : Term[A1] => a2 : Term[A2] => a3 : Term[A3] => Term[A4](FunctionDefinitionCall(
        definition = NativeFunctionDefinition(
            identity = f,
            signature = Signature(nameHint, typeA4.t, Seq(typeA1.t, typeA2.t, typeA3.t)),
            body = {case Seq(a, b, c) => f(Term[A1](a))(Term[A2](b))(Term[A3](c)).untyped}
        ),
        arguments = Seq(a1.untyped, a2.untyped, a3.untyped)
    ))}


    // Bind foreign functions

    def bindNative1[A1, A2](source : String)(implicit
        typeA1 : VariableType[Term[A1]],
        typeA2 : VariableType[Term[A2]]
    ) : Term[A1] => Term[A2] = {a1 : Term[A1] => Term[A2](FunctionDefinitionCall(
        definition = ForeignFunctionDefinition(
            source = source,
            returnType = typeA2.t,
            argumentTypes = Seq(typeA1.t)
        ),
        arguments = Seq(a1.untyped)
    ))}

    def bindNative2[A1, A2, A3](source : String)(implicit
        typeA1 : VariableType[Term[A1]],
        typeA2 : VariableType[Term[A2]],
        typeA3 : VariableType[Term[A3]]
    ) : Term[A1] => Term[A2] => Term[A3] = {a1 : Term[A1] => a2 : Term[A2] => Term[A3](FunctionDefinitionCall(
        definition = ForeignFunctionDefinition(
            source = source,
            returnType = typeA3.t,
            argumentTypes = Seq(typeA1.t, typeA2.t)
        ),
        arguments = Seq(a1.untyped, a2.untyped)
    ))}
}
