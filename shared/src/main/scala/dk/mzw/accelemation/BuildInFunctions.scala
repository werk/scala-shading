package dk.mzw.accelemation

import dk.mzw.accelemation.Internal.{Call, Untyped}
import dk.mzw.accelemation.Typed.{R, Vec2, Vec3, Vec4}

object BuildInFunctions {
    private def call(name : String, arguments : Typed[_]*) : Untyped = Call("radians", arguments.map(_.untyped).toList)

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

    /*    //vec3 sqrt(vec3 x)
        def sqrt : Typed[Double] = wrap(v)(Call("sqrt", List(untyped)))

        //vec3 inversesqrt(vec3 x)
        def inversesqrt : Typed[Double] = wrap(v)(Call("inversesqrt", List(untyped)))

        //vec3 abs(vec3 x)
        def abs : Typed[Double] = wrap(v)(Call("abs", List(untyped)))

        //vec3 sign(vec3 x)
        def sign : Typed[Double] = wrap(v)(Call("sign", List(untyped)))

        //vec3 floor(vec3 x)
        def floor : Typed[Double] = wrap(v)(Call("floor", List(untyped)))

        //vec3 ceil(vec3 x)
        def ceil : Typed[Double] = wrap(v)(Call("ceil", List(untyped)))

        //vec3 fract(vec3 x)
        def fract : Typed[Double] = wrap(v)(Call("fract", List(untyped)))

        //vec3 mod(vec3 x, vec3 y)
        def mod(y : Typed[Double]) : Typed[Double] = wrap(v)(Call("mod", List(untyped, y.untyped)))

        //vec3 min(vec3 x, vec3 y)
        def min(y : Typed[Double]) : Typed[Double] = wrap(v)(Call("min", List(untyped, y.untyped)))

        //vec3 max(vec3 x, vec3 y)
        def max(y : Typed[Double]) : Typed[Double] = wrap(v)(Call("max", List(untyped, y.untyped)))

        //vec3 clamp(vec3 x, vec3 minVal, vec3 maxVal)
        def clamp(minVal : Typed[Double], maxVal : Typed[Double]) : Typed[Double] = wrap(v)(Call("clamp", List(untyped, minVal.untyped, maxVal.untyped)))

        //vec3 mix(vec3 x, vec3 y, vec3 a)
        def mix(y : Typed[Double], a : Typed[Double]) : Typed[Double] = wrap(v)(Call("mix", List(untyped, y.untyped, a.untyped)))

        //vec3 step(vec3 edge, vec3 x)
        def step(edge : Typed[Double]) : Typed[Double] = wrap(v)(Call("step", List(edge.untyped, untyped)))

        //smoothstep(vec3 edge0, vec3 edge1, vec3 x)
        def smoothstep(edge0 : Typed[Double], edge1 : Typed[Double]) : Typed[Double] = wrap(v)(Call("smoothstep", List(edge0.untyped, edge1.untyped, untyped)))

        //float length(vec3 x)
        def length : Typed[Double] = wrap(v)(Call("length", List(untyped)))

        //float distance(vec3 p0, vec3 p1)
        def distance(p1 : Typed[Double]) : Typed[Double] = wrap(v)(Call("distance", List(untyped, p1.untyped)))

        //float dot(vec3 x, vec3 y)
        def dot : Typed[Double] = wrap(v)(Call("dot", List(untyped)))

        //vec3 normalize(vec3 x)
        def normalize : Typed[Double] = wrap(v)(Call("normalize", List(untyped)))

        //vec3 faceforward(vec3 N, vec3 I, vec3 Nref)
        def faceforward(I : Typed[Double], Nref : Typed[Double]) : Typed[Double] = wrap(v)(Call("faceforward", List(untyped, I.untyped, Nref.untyped)))

        //vec3 reflect(vec3 I, vec3 N)
        def reflect(N : Typed[Double]) : Typed[Double] = wrap(v)(Call("reflect", List(untyped, N.untyped)))

        //vec3 refract(vec3 I, vec3 N, float eta)
        def refract(N : Typed[Double], eta : R) : Typed[Double] = wrap(v)(Call("refract", List(untyped, N.untyped, eta.untyped)))
    */

}
