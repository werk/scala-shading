package dk.mzw.scalashading

import dk.mzw.scalashading.internal.Internal.{Call, Untyped}
import dk.mzw.scalashading.Language._

object Math {
    private def call(name : String, arguments : Typed[_]*) : Untyped = Call(name, arguments.map(untyped).toList)

    private def iterate[A <: Typed[_]](name : String, make : Untyped => A, x : A, y : A, zs : Seq[A]) : A = {
        make(zs.foldLeft(call(name, x, y)){case (m, a) => call(name, make(m), a)})
    }

    // TODO make global
    val pi : R = 3.141592653589793238462643383

    def radians(degrees : R) = R(call("radians", degrees))
    def radians(degrees : Vec2) = Vec2(call("radians", degrees))
    def radians(degrees : Vec3) = Vec3(call("radians", degrees))
    def radians(degrees : Vec4) = Vec4(call("radians", degrees))

    def degrees(radians : R) = R(call("degrees", radians))
    def degrees(radians : Vec2) = Vec2(call("degrees", radians))
    def degrees(radians : Vec3) = Vec3(call("degrees", radians))
    def degrees(radians : Vec4) = Vec4(call("degrees", radians))

    def sin(angle : R) = R(call("sin", angle))
    def sin(angle : Vec2) = Vec2(call("sin", angle))
    def sin(angle : Vec3) = Vec3(call("sin", angle))
    def sin(angle : Vec4) = Vec4(call("sin", angle))

    def cos(angle : R) = R(call("cos", angle))
    def cos(angle : Vec2) = Vec2(call("cos", angle))
    def cos(angle : Vec3) = Vec3(call("cos", angle))
    def cos(angle : Vec4) = Vec4(call("cos", angle))

    def tan(angle : R) = R(call("tan", angle))
    def tan(angle : Vec2) = Vec2(call("tan", angle))
    def tan(angle : Vec3) = Vec3(call("tan", angle))
    def tan(angle : Vec4) = Vec4(call("tan", angle))

    def asin(x : R) = R(call("asin", x))
    def asin(x : Vec2) = Vec2(call("asin", x))
    def asin(x : Vec3) = Vec3(call("asin", x))
    def asin(x : Vec4) = Vec4(call("asin", x))

    def acos(x : R) = R(call("acos", x))
    def acos(x : Vec2) = Vec2(call("acos", x))
    def acos(x : Vec3) = Vec3(call("acos", x))
    def acos(x : Vec4) = Vec4(call("acos", x))

    def atan(yOverX : R) = R(call("atan", yOverX))
    def atan(yOverX : Vec2) = Vec2(call("atan", yOverX))
    def atan(yOverX : Vec3) = Vec3(call("atan", yOverX))
    def atan(yOverX : Vec4) = Vec4(call("atan", yOverX))

    def atan(y : R, x : R) = R(call("atan", y, x))
    def atan(y : Vec2, x : Vec2) = Vec2(call("atan", y, x))
    def atan(y : Vec3, x : Vec3) = Vec3(call("atan", y, x))
    def atan(y : Vec4, x : Vec4) = Vec4(call("atan", y, x))

    def pow(x : R, y : R) = R(call("pow", x, y))
    def pow(x : Vec2, y : Vec2) = Vec2(call("pow", x, y))
    def pow(x : Vec3, y : Vec3) = Vec3(call("pow", x, y))
    def pow(x : Vec4, y : Vec4) = Vec4(call("pow", x, y))

    def exp(x : R) = R(call("exp", x))
    def exp(x : Vec2) = Vec2(call("exp", x))
    def exp(x : Vec3) = Vec3(call("exp", x))
    def exp(x : Vec4) = Vec4(call("exp", x))

    def log(x : R) = R(call("log", x))
    def log(x : Vec2) = Vec2(call("log", x))
    def log(x : Vec3) = Vec3(call("log", x))
    def log(x : Vec4) = Vec4(call("log", x))

    def exp2(x : R) = R(call("exp2", x))
    def exp2(x : Vec2) = Vec2(call("exp2", x))
    def exp2(x : Vec3) = Vec3(call("exp2", x))
    def exp2(x : Vec4) = Vec4(call("exp2", x))

    def log2(x : R) = R(call("log2", x))
    def log2(x : Vec2) = Vec2(call("log2", x))
    def log2(x : Vec3) = Vec3(call("log2", x))
    def log2(x : Vec4) = Vec4(call("log2", x))

    def sqrt(x : R) = R(call("sqrt", x))
    def sqrt(x : Vec2) = Vec2(call("sqrt", x))
    def sqrt(x : Vec3) = Vec3(call("sqrt", x))
    def sqrt(x : Vec4) = Vec4(call("sqrt", x))

    def inversesqrt(x : R) = R(call("inversesqrt", x))
    def inversesqrt(x : Vec2) = Vec2(call("inversesqrt", x))
    def inversesqrt(x : Vec3) = Vec3(call("inversesqrt", x))
    def inversesqrt(x : Vec4) = Vec4(call("inversesqrt", x))

    def abs(x : R) = R(call("abs", x))
    def abs(x : Vec2) = Vec2(call("abs", x))
    def abs(x : Vec3) = Vec3(call("abs", x))
    def abs(x : Vec4) = Vec4(call("abs", x))

    def sign(x : R) = R(call("sign", x))
    def sign(x : Vec2) = Vec2(call("sign", x))
    def sign(x : Vec3) = Vec3(call("sign", x))
    def sign(x : Vec4) = Vec4(call("sign", x))

    def floor(x : R) = R(call("floor", x))
    def floor(x : Vec2) = Vec2(call("floor", x))
    def floor(x : Vec3) = Vec3(call("floor", x))
    def floor(x : Vec4) = Vec4(call("floor", x))

    def ceil(x : R) = R(call("ceil", x))
    def ceil(x : Vec2) = Vec2(call("ceil", x))
    def ceil(x : Vec3) = Vec3(call("ceil", x))
    def ceil(x : Vec4) = Vec4(call("ceil", x))

    def fract(x : R) = R(call("fract", x))
    def fract(x : Vec2) = Vec2(call("fract", x))
    def fract(x : Vec3) = Vec3(call("fract", x))
    def fract(x : Vec4) = Vec4(call("fract", x))

    def mod(x : R, y : R) = R(call("mod", x, y))
    def mod(x : Vec2, y : Vec2) = Vec2(call("mod", x, y))
    def mod(x : Vec3, y : Vec3) = Vec3(call("mod", x, y))
    def mod(x : Vec4, y : Vec4) = Vec4(call("mod", x, y))

    def min(x : R, y : R, zs : R*) = iterate("min", R(_), x, y, zs)
    def min(x : Vec2, y : Vec2, zs : Vec2*) = iterate("min", Vec2(_), x, y, zs)
    def min(x : Vec3, y : Vec3, zs : Vec3*) = iterate("min", Vec3(_), x, y, zs)
    def min(x : Vec4, y : Vec4, zs : Vec4*) = iterate("min", Vec4(_), x, y, zs)

    def max(x : R, y : R, zs : R*) = iterate("max", R(_), x, y, zs)
    def max(x : Vec2, y : Vec2, zs : Vec2*) = iterate("max", Vec2(_), x, y, zs)
    def max(x : Vec3, y : Vec3, zs : Vec3*) = iterate("max", Vec3(_), x, y, zs)
    def max(x : Vec4, y : Vec4, zs : Vec4*) = iterate("max", Vec4(_), x, y, zs)

    def clamp(x : R, minVal : R, maxVal : R) = R(call("clamp", x, minVal, maxVal))
    def clamp(x : Vec2, minVal : Vec2, maxVal : Vec2) = Vec2(call("clamp", x, minVal, maxVal))
    def clamp(x : Vec3, minVal : Vec3, maxVal : Vec3) = Vec3(call("clamp", x, minVal, maxVal))
    def clamp(x : Vec4, minVal : Vec4, maxVal : Vec4) = Vec4(call("clamp", x, minVal, maxVal))
    def clamp(x : Vec2, minVal : R, maxVal : R) = Vec2(call("clamp", x, minVal, maxVal))
    def clamp(x : Vec3, minVal : R, maxVal : R) = Vec3(call("clamp", x, minVal, maxVal))
    def clamp(x : Vec4, minVal : R, maxVal : R) = Vec4(call("clamp", x, minVal, maxVal))

    def mix(x : R, y : R, a : R) = R(call("mix", x, y, a))
    def mix(x : Vec2, y : Vec2, a : Vec2) = Vec2(call("mix", x, y, a))
    def mix(x : Vec3, y : Vec3, a : Vec3) = Vec3(call("mix", x, y, a))
    def mix(x : Vec4, y : Vec4, a : Vec4) = Vec4(call("mix", x, y, a))
    def mix(x : Vec2, y : Vec2, a : R) = Vec2(call("mix", x, y, a))
    def mix(x : Vec3, y : Vec3, a : R) = Vec3(call("mix", x, y, a))
    def mix(x : Vec4, y : Vec4, a : R) = Vec4(call("mix", x, y, a))

    def step(edge : R, x : R) = R(call("step", edge, x))
    def step(edge : Vec2, x : Vec2) = Vec2(call("step", edge, x))
    def step(edge : Vec3, x : Vec3) = Vec3(call("step", edge, x))
    def step(edge : Vec4, x : Vec4) = Vec4(call("step", edge, x))
    def step(edge : Vec2, x : R) = Vec2(call("step", edge, x))
    def step(edge : Vec3, x : R) = Vec3(call("step", edge, x))
    def step(edge : Vec4, x : R) = Vec4(call("step", edge, x))

    def smoothstep(edge0 : R, edge1 : R, x : R) = R(call("smoothstep", edge0, edge1, x))
    def smoothstep(edge0 : Vec2, edge1 : Vec2, x : Vec2) = Vec2(call("smoothstep", edge0, edge1, x))
    def smoothstep(edge0 : Vec3, edge1 : Vec3, x : Vec3) = Vec3(call("smoothstep", edge0, edge1, x))
    def smoothstep(edge0 : Vec4, edge1 : Vec4, x : Vec4) = Vec4(call("smoothstep", edge0, edge1, x))
    def smoothstep(edge0 : R, edge1 : R, x : Vec2) = Vec2(call("smoothstep", edge0, edge1, x))
    def smoothstep(edge0 : R, edge1 : R, x : Vec3) = Vec3(call("smoothstep", edge0, edge1, x))
    def smoothstep(edge0 : R, edge1 : R, x : Vec4) = Vec4(call("smoothstep", edge0, edge1, x))

    def length(x : R) = R(call("length", x))
    def length(x : Vec2) = R(call("length", x))
    def length(x : Vec3) = R(call("length", x))
    def length(x : Vec4) = R(call("length", x))

    def distance(p1 : R, p2 : R) = R(call("distance", p1, p2))
    def distance(p1 : Vec2, p2 : Vec2) = R(call("distance", p1, p2))
    def distance(p1 : Vec3, p2 : Vec3) = R(call("distance", p1, p2))
    def distance(p1 : Vec4, p2 : Vec4) = R(call("distance", p1, p2))

    def dot(x : R, y : R) = R(call("dot", x, y))
    def dot(x : Vec2, y : Vec2) = R(call("dot", x, y))
    def dot(x : Vec3, y : Vec3) = R(call("dot", x, y))
    def dot(x : Vec4, y : Vec4) = R(call("dot", x, y))

    def cross(x : Vec3, y : Vec3) = Vec3(call("cross", x, y))

    def normalize(x : R) = R(call("normalize", x))
    def normalize(x : Vec2) = Vec2(call("normalize", x))
    def normalize(x : Vec3) = Vec3(call("normalize", x))
    def normalize(x : Vec4) = Vec4(call("normalize", x))

    def faceforward(n : R, i: R, nRef : R) = R(call("faceforward", n, i, nRef))
    def faceforward(n : Vec2, i: Vec2, nRef : Vec2) = Vec2(call("faceforward", n, i, nRef))
    def faceforward(n : Vec3, i: Vec3, nRef : Vec3) = Vec3(call("faceforward", n, i, nRef))
    def faceforward(n : Vec4, i: Vec4, nRef : Vec4) = Vec4(call("faceforward", n, i, nRef))

    def reflect(n : R, i: R) = R(call("reflect", n, i))
    def reflect(n : Vec2, i: Vec2) = Vec2(call("reflect", n, i))
    def reflect(n : Vec3, i: Vec3) = Vec3(call("reflect", n, i))
    def reflect(n : Vec4, i: Vec4) = Vec4(call("reflect", n, i))

    def refract(n : R, i: R, eta : R) = R(call("refract", n, i, eta))
    def refract(n : Vec2, i: Vec2, eta : R) = Vec2(call("refract", n, i, eta))
    def refract(n : Vec3, i: Vec3, eta : R) = Vec3(call("refract", n, i, eta))
    def refract(n : Vec4, i: Vec4, eta : R) = Vec4(call("refract", n, i, eta))

}
