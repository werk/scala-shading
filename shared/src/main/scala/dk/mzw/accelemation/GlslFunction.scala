package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language._

// Reference http://www.shaderific.com/glsl-functions/
object GlslFunction {

    case class IsReal234[T]()
    implicit def vec2IsReal234 = IsReal234[Vec2]()
    implicit def vec3IsReal234 = IsReal234[Vec3]()
    implicit def vec4IsReal234 = IsReal234[Vec4]()

    case class IsReal1234[T]()
    implicit def vec1IsReal1234 = IsReal1234[R]()
    implicit def vec234IsReal1234[T](implicit evidence : IsReal234[T]) = IsReal1234[T]()

    implicit class RealVectorWithOperations[T](my : Term[T])(implicit evidence: IsReal1234[Term[T]]) {
        //vec3 radians(vec3 degrees)
        def radians : Term[T] = Term(Call("radians", List(my.untyped)))

        //vec3 degrees(vec3 radians)
        def degrees : Term[T] = Term(Call("degrees", List(my.untyped)))

        //vec3 sin(vec3 angle)
        def sin : Term[T] = Term(Call("sin", List(my.untyped)))

        //vec3 cos(vec3 angle)
        def cos : Term[T] = Term(Call("cos", List(my.untyped)))

        //vec3 tan(vec3 angle)
        def tan : Term[T] = Term(Call("tan", List(my.untyped)))

        //vec3 asin(vec3 x)
        def asin : Term[T] = Term(Call("asin", List(my.untyped)))

        //vec3 acos(vec3 x)
        def acos : Term[T] = Term(Call("acos", List(my.untyped)))

        //vec3 atan(vec3 y_over_x)
        def atan : Term[T] = Term(Call("atan", List(my.untyped)))

        //vec3 atan(vec3 y, vec3 x)
        def atan(x : Term[T]) : Term[T] = Term(Call("atan", List(my.untyped, x.untyped)))

        //vec3 pow(vec3 x, vec3 y)
        def pow(y : Term[T]) : Term[T] = Term(Call("pow", List(my.untyped, y.untyped)))

        //vec3 exp(vec3 x)
        def exp : Term[T] = Term(Call("exp", List(my.untyped)))

        //vec3 log(vec3 x)
        def log : Term[T] = Term(Call("log", List(my.untyped)))

        //vec3 exp2(vec3 x)
        def exp2 : Term[T] = Term(Call("exp2", List(my.untyped)))

        //vec3 log2(vec3 x)
        def log2 : Term[T] = Term(Call("log2", List(my.untyped)))

        //vec3 sqrt(vec3 x)
        def sqrt : Term[T] = Term(Call("sqrt", List(my.untyped)))

        //vec3 inversesqrt(vec3 x)
        def inversesqrt : Term[T] = Term(Call("inversesqrt", List(my.untyped)))

        //vec3 abs(vec3 x)
        def abs : Term[T] = Term(Call("abs", List(my.untyped)))

        //vec3 sign(vec3 x)
        def sign : Term[T] = Term(Call("sign", List(my.untyped)))

        //vec3 floor(vec3 x)
        def floor : Term[T] = Term(Call("floor", List(my.untyped)))

        //vec3 ceil(vec3 x)
        def ceil : Term[T] = Term(Call("ceil", List(my.untyped)))

        //vec3 fract(vec3 x)
        def fract : Term[T] = Term(Call("fract", List(my.untyped)))

        //vec3 mod(vec3 x, vec3 y)
        def mod(y : Term[T]) : Term[T] = Term(Call("mod", List(my.untyped, y.untyped)))

        //vec3 min(vec3 x, vec3 y)
        def min(y : Term[T]) : Term[T] = Term(Call("min", List(my.untyped, y.untyped)))

        //vec3 max(vec3 x, vec3 y)
        def max(y : Term[T]) : Term[T] = Term(Call("max", List(my.untyped, y.untyped)))

        //vec3 clamp(vec3 x, vec3 minVal, vec3 maxVal)
        def clamp(minVal : Term[T], maxVal : Term[T]) : Term[T] = Term(Call("clamp", List(my.untyped, minVal.untyped, maxVal.untyped)))

        //vec3 mix(vec3 x, vec3 y, vec3 a)
        def mix(y : Term[T], a : Term[T]) : Term[T] = Term(Call("mix", List(my.untyped, y.untyped, a.untyped)))

        //vec3 step(vec3 edge, vec3 x)
        def step(edge : Term[T]) : Term[T] = Term(Call("step", List(edge.untyped, my.untyped)))

        //smoothstep(vec3 edge0, vec3 edge1, vec3 x)
        def smoothstep(edge0 : Term[T], edge1 : Term[T]) : Term[T] = Term(Call("smoothstep", List(edge0.untyped, edge1.untyped, my.untyped)))

        //float length(vec3 x)
        def length : Term[T] = Term(Call("length", List(my.untyped)))

        //float distance(vec3 p0, vec3 p1)
        def distance(p1 : Term[T]) : Term[T] = Term(Call("distance", List(my.untyped, p1.untyped)))

        //float dot(vec3 x, vec3 y)
        def dot : Term[T] = Term(Call("dot", List(my.untyped)))

        //vec3 normalize(vec3 x)
        def normalize : Term[T] = Term(Call("normalize", List(my.untyped)))

        //vec3 faceforward(vec3 N, vec3 I, vec3 Nref)
        def faceforward(I : Term[T], Nref : Term[T]) : Term[T] = Term(Call("faceforward", List(my.untyped, I.untyped, Nref.untyped)))

        //vec3 reflect(vec3 I, vec3 N)
        def reflect(N : Term[T]) : Term[T] = Term(Call("reflect", List(my.untyped, N.untyped)))

        //vec3 refract(vec3 I, vec3 N, float eta)
        def refract(N : Term[T], eta : R) : Term[T] = Term(Call("refract", List(my.untyped, N.untyped, eta.untyped)))
    }

    implicit class Real234VectorWithOperations[T](my : Term[T])(implicit evidence: IsReal234[Term[T]]) {
        //vec3 mod(vec3 x, float y)
        def mod(y : R) : Term[T] = Term(Call("mod", List(my.untyped, y.untyped)))

        //vec3 min(vec3 x, float y)
        def min(y : R) : Term[T] = Term(Call("min", List(my.untyped, y.untyped)))

        //vec3 max(vec3 x, float y)
        def max(y : R) : Term[T] = Term(Call("max", List(my.untyped, y.untyped)))

        //vec3 clamp(vec3 x, float minVal, float maxVal)
        def clamp(minVal : R, maxVal : R) : Term[T] = Term(Call("clamp", List(my.untyped, minVal.untyped, maxVal.untyped)))

        //vec3 mix(vec3 x, vec3 y, float a)
        def mix(y : Term[T], a : R) : Term[T] = Term(Call("mix", List(my.untyped, y.untyped, a.untyped)))

        //vec3 step(float edge, vec3 x)
        def step(edge : R) : Term[T] = Term(Call("step", List(edge.untyped, my.untyped)))

        //smoothstep(float edge0, float edge1, vec3 x)
        def smoothstep(edge0 : R, edge1 : R) : Term[T] = Term(Call("smoothstep", List(edge0.untyped, edge1.untyped, my.untyped)))


        //bvec3 lessThan(vec3 x, vec3 y)
        //bvec3 lessThanEqual(vec3 x, vec3 y)
        //bvec3 greaterThan(vec3 x, vec3 y)
        //bvec3 greaterThanEqual(vec3 x, vec3 y)
        //bvec3 equal(vec3 x, vec3 y)
        //bvec3 notEqual(vec3 x, vec3 y)
        //bool any(bvec3 x)
        //bool all(bvec3 x)
        //bvec3 not(bvec3 x)
    }

    implicit class Real3VectorWithOperations[T](a : Vec3) {
        def cross(y : Vec3) : Term[T] = Term(Call("cross", List(a.untyped, y.untyped)))
    }


}
