package dk.mzw.accelemation

import dk.mzw.accelemation.Internal._

object Typed {

    sealed abstract class Typed[T] {
        type Self <: Typed[T]

        val untyped : Untyped
        val make : Untyped => Self
        val typeName : String

        def ===(b : Self) : B = B(Infix("==", untyped, b.untyped))
        def !=(b : Self) : B = B(Infix("!=", untyped, b.untyped))

        def bind[B](f : Typed[T] => Typed[B]) : Typed[B] = {
            def body(a: Untyped) : Untyped = f(make(a)).untyped
            val makeB : Untyped => Typed[B] = null // TODO
            makeB(Bind(typeName, untyped, body))
        }
    }

    trait Vec extends Typed[Double] with VecOrR {
        def magnitude : R = R(Call("length", List(untyped)))
        def normalize : Self = make(Call("length", List(untyped)))
    }

    trait VecOrR extends Typed[Double] {
        def +(b : Self) : Self = make(Infix("+", untyped, b.untyped))
        def -(b : Self) : Self = make(Infix("-", untyped, b.untyped))
        def unary_-() : Self = make(Prefix("-", untyped))
        def *(b : Self) : Self = make(Infix("*", untyped, b.untyped))
        def /(b : Self) : Self = make(Infix("/", untyped, b.untyped))
    }

    // Concrete types


    // GLSL float
    case class R(untyped : Untyped) extends Typed[Double] with VecOrR {
        type Self = R
        val make = copy _
        val typeName = "float"

        def <(b : R) : B = B(Infix("<", untyped, b.untyped))
        def >(b : R) : B = B(Infix(">", untyped, b.untyped))
        def <=(b : R) : B = B(Infix("<=", untyped, b.untyped))
        def >=(b : R) : B = B(Infix(">=", untyped, b.untyped))
    }

    // GLSL bool
    case class B(untyped : Untyped) extends Typed[Boolean] {
        type Self = B
        val make = copy _
        val typeName = "bool"

        def unary_!() : B = B(Prefix("!", untyped))
        def &&(b : B) : B = B(Infix("&&", untyped, b.untyped))
        def ||(b : B) : B = B(Infix("||", untyped, b.untyped))
    }

    // GLSL int
    case class I(untyped : Untyped) extends Typed[Int] {
        type Self = I
        val make = copy _
        val typeName = "int"
    }

    case class Vec2(untyped : Untyped) extends Typed[Double] with Vec {
        type Self = Vec2
        val make = copy _
        val typeName = "vec2"

        def x = R(Field("x", untyped))
        def y = R(Field("y", untyped))
        
        def xx = Vec2(Field("xx", untyped))
        def xy = Vec2(Field("xy", untyped))
        def yx = Vec2(Field("yx", untyped))
        def yy = Vec2(Field("yy", untyped))
    }

    case class Vec3(untyped : Untyped) extends Typed[Double] with Vec {
        type Self = Vec3
        val make = copy _
        val typeName = "vec3"

        def x : R = R(Field("x", untyped))
        def y : R = R(Field("y", untyped))
        def z : R = R(Field("z", untyped))

        def xx : Vec2 = Vec2(Field("xx", untyped))
        def xy : Vec2 = Vec2(Field("xy", untyped))
        def xz : Vec2 = Vec2(Field("xz", untyped))
        def yx : Vec2 = Vec2(Field("yx", untyped))
        def yy : Vec2 = Vec2(Field("yy", untyped))
        def yz : Vec2 = Vec2(Field("yz", untyped))
        def zx : Vec2 = Vec2(Field("zx", untyped))
        def zy : Vec2 = Vec2(Field("zy", untyped))
        def zz : Vec2 = Vec2(Field("zz", untyped))

        def xxx : Vec3 = Vec3(Field("xxx", untyped))
        def xxy : Vec3 = Vec3(Field("xxy", untyped))
        def xxz : Vec3 = Vec3(Field("xxz", untyped))
        def xyx : Vec3 = Vec3(Field("xyx", untyped))
        def xyy : Vec3 = Vec3(Field("xyy", untyped))
        def xyz : Vec3 = Vec3(Field("xyz", untyped))
        def xzx : Vec3 = Vec3(Field("xzx", untyped))
        def xzy : Vec3 = Vec3(Field("xzy", untyped))
        def xzz : Vec3 = Vec3(Field("xzz", untyped))
        def yxx : Vec3 = Vec3(Field("yxx", untyped))
        def yxy : Vec3 = Vec3(Field("yxy", untyped))
        def yxz : Vec3 = Vec3(Field("yxz", untyped))
        def yyx : Vec3 = Vec3(Field("yyx", untyped))
        def yyy : Vec3 = Vec3(Field("yyy", untyped))
        def yyz : Vec3 = Vec3(Field("yyz", untyped))
        def yzx : Vec3 = Vec3(Field("yzx", untyped))
        def yzy : Vec3 = Vec3(Field("yzy", untyped))
        def yzz : Vec3 = Vec3(Field("yzz", untyped))
        def zxx : Vec3 = Vec3(Field("zxx", untyped))
        def zxy : Vec3 = Vec3(Field("zxy", untyped))
        def zxz : Vec3 = Vec3(Field("zxz", untyped))
        def zyx : Vec3 = Vec3(Field("zyx", untyped))
        def zyy : Vec3 = Vec3(Field("zyy", untyped))
        def zyz : Vec3 = Vec3(Field("zyz", untyped))
        def zzx : Vec3 = Vec3(Field("zzx", untyped))
        def zzy : Vec3 = Vec3(Field("zzy", untyped))
        def zzz : Vec3 = Vec3(Field("zzz", untyped))        
    }

    case class Vec4(untyped : Untyped) extends Typed[Double] with Vec {
        type Self = Vec4
        val make = copy _
        val typeName = "vec4"

        def x : R = R(Field("x", untyped))
        def y : R = R(Field("y", untyped))
        def z : R = R(Field("z", untyped))
        def w : R = R(Field("w", untyped))

        def xx = Vec2(Field("xx", untyped))
        def xy = Vec2(Field("xy", untyped))
        def xz = Vec2(Field("xz", untyped))
        def xw = Vec2(Field("xw", untyped))
        def yx = Vec2(Field("yx", untyped))
        def yy = Vec2(Field("yy", untyped))
        def yz = Vec2(Field("yz", untyped))
        def yw = Vec2(Field("yw", untyped))
        def zx = Vec2(Field("zx", untyped))
        def zy = Vec2(Field("zy", untyped))
        def zz = Vec2(Field("zz", untyped))
        def zw = Vec2(Field("zw", untyped))
        def wx = Vec2(Field("wx", untyped))
        def wy = Vec2(Field("wy", untyped))
        def wz = Vec2(Field("wz", untyped))
        def ww = Vec2(Field("ww", untyped))

        def xxx = Vec3(Field("xxx", untyped))
        def xxy = Vec3(Field("xxy", untyped))
        def xxz = Vec3(Field("xxz", untyped))
        def xxw = Vec3(Field("xxw", untyped))
        def xyx = Vec3(Field("xyx", untyped))
        def xyy = Vec3(Field("xyy", untyped))
        def xyz = Vec3(Field("xyz", untyped))
        def xyw = Vec3(Field("xyw", untyped))
        def xzx = Vec3(Field("xzx", untyped))
        def xzy = Vec3(Field("xzy", untyped))
        def xzz = Vec3(Field("xzz", untyped))
        def xzw = Vec3(Field("xzw", untyped))
        def xwx = Vec3(Field("xwx", untyped))
        def xwy = Vec3(Field("xwy", untyped))
        def xwz = Vec3(Field("xwz", untyped))
        def xww = Vec3(Field("xww", untyped))
        def yxx = Vec3(Field("yxx", untyped))
        def yxy = Vec3(Field("yxy", untyped))
        def yxz = Vec3(Field("yxz", untyped))
        def yxw = Vec3(Field("yxw", untyped))
        def yyx = Vec3(Field("yyx", untyped))
        def yyy = Vec3(Field("yyy", untyped))
        def yyz = Vec3(Field("yyz", untyped))
        def yyw = Vec3(Field("yyw", untyped))
        def yzx = Vec3(Field("yzx", untyped))
        def yzy = Vec3(Field("yzy", untyped))
        def yzz = Vec3(Field("yzz", untyped))
        def yzw = Vec3(Field("yzw", untyped))
        def ywx = Vec3(Field("ywx", untyped))
        def ywy = Vec3(Field("ywy", untyped))
        def ywz = Vec3(Field("ywz", untyped))
        def yww = Vec3(Field("yww", untyped))
        def zxx = Vec3(Field("zxx", untyped))
        def zxy = Vec3(Field("zxy", untyped))
        def zxz = Vec3(Field("zxz", untyped))
        def zxw = Vec3(Field("zxw", untyped))
        def zyx = Vec3(Field("zyx", untyped))
        def zyy = Vec3(Field("zyy", untyped))
        def zyz = Vec3(Field("zyz", untyped))
        def zyw = Vec3(Field("zyw", untyped))
        def zzx = Vec3(Field("zzx", untyped))
        def zzy = Vec3(Field("zzy", untyped))
        def zzz = Vec3(Field("zzz", untyped))
        def zzw = Vec3(Field("zzw", untyped))
        def zwx = Vec3(Field("zwx", untyped))
        def zwy = Vec3(Field("zwy", untyped))
        def zwz = Vec3(Field("zwz", untyped))
        def zww = Vec3(Field("zww", untyped))
        def wxx = Vec3(Field("wxx", untyped))
        def wxy = Vec3(Field("wxy", untyped))
        def wxz = Vec3(Field("wxz", untyped))
        def wxw = Vec3(Field("wxw", untyped))
        def wyx = Vec3(Field("wyx", untyped))
        def wyy = Vec3(Field("wyy", untyped))
        def wyz = Vec3(Field("wyz", untyped))
        def wyw = Vec3(Field("wyw", untyped))
        def wzx = Vec3(Field("wzx", untyped))
        def wzy = Vec3(Field("wzy", untyped))
        def wzz = Vec3(Field("wzz", untyped))
        def wzw = Vec3(Field("wzw", untyped))
        def wwx = Vec3(Field("wwx", untyped))
        def wwy = Vec3(Field("wwy", untyped))
        def wwz = Vec3(Field("wwz", untyped))
        def www = Vec3(Field("www", untyped))

        def xxxx = Vec4(Field("xxxx", untyped))
        def xxxy = Vec4(Field("xxxy", untyped))
        def xxxz = Vec4(Field("xxxz", untyped))
        def xxxw = Vec4(Field("xxxw", untyped))
        def xxyx = Vec4(Field("xxyx", untyped))
        def xxyy = Vec4(Field("xxyy", untyped))
        def xxyz = Vec4(Field("xxyz", untyped))
        def xxyw = Vec4(Field("xxyw", untyped))
        def xxzx = Vec4(Field("xxzx", untyped))
        def xxzy = Vec4(Field("xxzy", untyped))
        def xxzz = Vec4(Field("xxzz", untyped))
        def xxzw = Vec4(Field("xxzw", untyped))
        def xxwx = Vec4(Field("xxwx", untyped))
        def xxwy = Vec4(Field("xxwy", untyped))
        def xxwz = Vec4(Field("xxwz", untyped))
        def xxww = Vec4(Field("xxww", untyped))
        def xyxx = Vec4(Field("xyxx", untyped))
        def xyxy = Vec4(Field("xyxy", untyped))
        def xyxz = Vec4(Field("xyxz", untyped))
        def xyxw = Vec4(Field("xyxw", untyped))
        def xyyx = Vec4(Field("xyyx", untyped))
        def xyyy = Vec4(Field("xyyy", untyped))
        def xyyz = Vec4(Field("xyyz", untyped))
        def xyyw = Vec4(Field("xyyw", untyped))
        def xyzx = Vec4(Field("xyzx", untyped))
        def xyzy = Vec4(Field("xyzy", untyped))
        def xyzz = Vec4(Field("xyzz", untyped))
        def xyzw = Vec4(Field("xyzw", untyped))
        def xywx = Vec4(Field("xywx", untyped))
        def xywy = Vec4(Field("xywy", untyped))
        def xywz = Vec4(Field("xywz", untyped))
        def xyww = Vec4(Field("xyww", untyped))
        def xzxx = Vec4(Field("xzxx", untyped))
        def xzxy = Vec4(Field("xzxy", untyped))
        def xzxz = Vec4(Field("xzxz", untyped))
        def xzxw = Vec4(Field("xzxw", untyped))
        def xzyx = Vec4(Field("xzyx", untyped))
        def xzyy = Vec4(Field("xzyy", untyped))
        def xzyz = Vec4(Field("xzyz", untyped))
        def xzyw = Vec4(Field("xzyw", untyped))
        def xzzx = Vec4(Field("xzzx", untyped))
        def xzzy = Vec4(Field("xzzy", untyped))
        def xzzz = Vec4(Field("xzzz", untyped))
        def xzzw = Vec4(Field("xzzw", untyped))
        def xzwx = Vec4(Field("xzwx", untyped))
        def xzwy = Vec4(Field("xzwy", untyped))
        def xzwz = Vec4(Field("xzwz", untyped))
        def xzww = Vec4(Field("xzww", untyped))
        def xwxx = Vec4(Field("xwxx", untyped))
        def xwxy = Vec4(Field("xwxy", untyped))
        def xwxz = Vec4(Field("xwxz", untyped))
        def xwxw = Vec4(Field("xwxw", untyped))
        def xwyx = Vec4(Field("xwyx", untyped))
        def xwyy = Vec4(Field("xwyy", untyped))
        def xwyz = Vec4(Field("xwyz", untyped))
        def xwyw = Vec4(Field("xwyw", untyped))
        def xwzx = Vec4(Field("xwzx", untyped))
        def xwzy = Vec4(Field("xwzy", untyped))
        def xwzz = Vec4(Field("xwzz", untyped))
        def xwzw = Vec4(Field("xwzw", untyped))
        def xwwx = Vec4(Field("xwwx", untyped))
        def xwwy = Vec4(Field("xwwy", untyped))
        def xwwz = Vec4(Field("xwwz", untyped))
        def xwww = Vec4(Field("xwww", untyped))
        def yxxx = Vec4(Field("yxxx", untyped))
        def yxxy = Vec4(Field("yxxy", untyped))
        def yxxz = Vec4(Field("yxxz", untyped))
        def yxxw = Vec4(Field("yxxw", untyped))
        def yxyx = Vec4(Field("yxyx", untyped))
        def yxyy = Vec4(Field("yxyy", untyped))
        def yxyz = Vec4(Field("yxyz", untyped))
        def yxyw = Vec4(Field("yxyw", untyped))
        def yxzx = Vec4(Field("yxzx", untyped))
        def yxzy = Vec4(Field("yxzy", untyped))
        def yxzz = Vec4(Field("yxzz", untyped))
        def yxzw = Vec4(Field("yxzw", untyped))
        def yxwx = Vec4(Field("yxwx", untyped))
        def yxwy = Vec4(Field("yxwy", untyped))
        def yxwz = Vec4(Field("yxwz", untyped))
        def yxww = Vec4(Field("yxww", untyped))
        def yyxx = Vec4(Field("yyxx", untyped))
        def yyxy = Vec4(Field("yyxy", untyped))
        def yyxz = Vec4(Field("yyxz", untyped))
        def yyxw = Vec4(Field("yyxw", untyped))
        def yyyx = Vec4(Field("yyyx", untyped))
        def yyyy = Vec4(Field("yyyy", untyped))
        def yyyz = Vec4(Field("yyyz", untyped))
        def yyyw = Vec4(Field("yyyw", untyped))
        def yyzx = Vec4(Field("yyzx", untyped))
        def yyzy = Vec4(Field("yyzy", untyped))
        def yyzz = Vec4(Field("yyzz", untyped))
        def yyzw = Vec4(Field("yyzw", untyped))
        def yywx = Vec4(Field("yywx", untyped))
        def yywy = Vec4(Field("yywy", untyped))
        def yywz = Vec4(Field("yywz", untyped))
        def yyww = Vec4(Field("yyww", untyped))
        def yzxx = Vec4(Field("yzxx", untyped))
        def yzxy = Vec4(Field("yzxy", untyped))
        def yzxz = Vec4(Field("yzxz", untyped))
        def yzxw = Vec4(Field("yzxw", untyped))
        def yzyx = Vec4(Field("yzyx", untyped))
        def yzyy = Vec4(Field("yzyy", untyped))
        def yzyz = Vec4(Field("yzyz", untyped))
        def yzyw = Vec4(Field("yzyw", untyped))
        def yzzx = Vec4(Field("yzzx", untyped))
        def yzzy = Vec4(Field("yzzy", untyped))
        def yzzz = Vec4(Field("yzzz", untyped))
        def yzzw = Vec4(Field("yzzw", untyped))
        def yzwx = Vec4(Field("yzwx", untyped))
        def yzwy = Vec4(Field("yzwy", untyped))
        def yzwz = Vec4(Field("yzwz", untyped))
        def yzww = Vec4(Field("yzww", untyped))
        def ywxx = Vec4(Field("ywxx", untyped))
        def ywxy = Vec4(Field("ywxy", untyped))
        def ywxz = Vec4(Field("ywxz", untyped))
        def ywxw = Vec4(Field("ywxw", untyped))
        def ywyx = Vec4(Field("ywyx", untyped))
        def ywyy = Vec4(Field("ywyy", untyped))
        def ywyz = Vec4(Field("ywyz", untyped))
        def ywyw = Vec4(Field("ywyw", untyped))
        def ywzx = Vec4(Field("ywzx", untyped))
        def ywzy = Vec4(Field("ywzy", untyped))
        def ywzz = Vec4(Field("ywzz", untyped))
        def ywzw = Vec4(Field("ywzw", untyped))
        def ywwx = Vec4(Field("ywwx", untyped))
        def ywwy = Vec4(Field("ywwy", untyped))
        def ywwz = Vec4(Field("ywwz", untyped))
        def ywww = Vec4(Field("ywww", untyped))
        def zxxx = Vec4(Field("zxxx", untyped))
        def zxxy = Vec4(Field("zxxy", untyped))
        def zxxz = Vec4(Field("zxxz", untyped))
        def zxxw = Vec4(Field("zxxw", untyped))
        def zxyx = Vec4(Field("zxyx", untyped))
        def zxyy = Vec4(Field("zxyy", untyped))
        def zxyz = Vec4(Field("zxyz", untyped))
        def zxyw = Vec4(Field("zxyw", untyped))
        def zxzx = Vec4(Field("zxzx", untyped))
        def zxzy = Vec4(Field("zxzy", untyped))
        def zxzz = Vec4(Field("zxzz", untyped))
        def zxzw = Vec4(Field("zxzw", untyped))
        def zxwx = Vec4(Field("zxwx", untyped))
        def zxwy = Vec4(Field("zxwy", untyped))
        def zxwz = Vec4(Field("zxwz", untyped))
        def zxww = Vec4(Field("zxww", untyped))
        def zyxx = Vec4(Field("zyxx", untyped))
        def zyxy = Vec4(Field("zyxy", untyped))
        def zyxz = Vec4(Field("zyxz", untyped))
        def zyxw = Vec4(Field("zyxw", untyped))
        def zyyx = Vec4(Field("zyyx", untyped))
        def zyyy = Vec4(Field("zyyy", untyped))
        def zyyz = Vec4(Field("zyyz", untyped))
        def zyyw = Vec4(Field("zyyw", untyped))
        def zyzx = Vec4(Field("zyzx", untyped))
        def zyzy = Vec4(Field("zyzy", untyped))
        def zyzz = Vec4(Field("zyzz", untyped))
        def zyzw = Vec4(Field("zyzw", untyped))
        def zywx = Vec4(Field("zywx", untyped))
        def zywy = Vec4(Field("zywy", untyped))
        def zywz = Vec4(Field("zywz", untyped))
        def zyww = Vec4(Field("zyww", untyped))
        def zzxx = Vec4(Field("zzxx", untyped))
        def zzxy = Vec4(Field("zzxy", untyped))
        def zzxz = Vec4(Field("zzxz", untyped))
        def zzxw = Vec4(Field("zzxw", untyped))
        def zzyx = Vec4(Field("zzyx", untyped))
        def zzyy = Vec4(Field("zzyy", untyped))
        def zzyz = Vec4(Field("zzyz", untyped))
        def zzyw = Vec4(Field("zzyw", untyped))
        def zzzx = Vec4(Field("zzzx", untyped))
        def zzzy = Vec4(Field("zzzy", untyped))
        def zzzz = Vec4(Field("zzzz", untyped))
        def zzzw = Vec4(Field("zzzw", untyped))
        def zzwx = Vec4(Field("zzwx", untyped))
        def zzwy = Vec4(Field("zzwy", untyped))
        def zzwz = Vec4(Field("zzwz", untyped))
        def zzww = Vec4(Field("zzww", untyped))
        def zwxx = Vec4(Field("zwxx", untyped))
        def zwxy = Vec4(Field("zwxy", untyped))
        def zwxz = Vec4(Field("zwxz", untyped))
        def zwxw = Vec4(Field("zwxw", untyped))
        def zwyx = Vec4(Field("zwyx", untyped))
        def zwyy = Vec4(Field("zwyy", untyped))
        def zwyz = Vec4(Field("zwyz", untyped))
        def zwyw = Vec4(Field("zwyw", untyped))
        def zwzx = Vec4(Field("zwzx", untyped))
        def zwzy = Vec4(Field("zwzy", untyped))
        def zwzz = Vec4(Field("zwzz", untyped))
        def zwzw = Vec4(Field("zwzw", untyped))
        def zwwx = Vec4(Field("zwwx", untyped))
        def zwwy = Vec4(Field("zwwy", untyped))
        def zwwz = Vec4(Field("zwwz", untyped))
        def zwww = Vec4(Field("zwww", untyped))
        def wxxx = Vec4(Field("wxxx", untyped))
        def wxxy = Vec4(Field("wxxy", untyped))
        def wxxz = Vec4(Field("wxxz", untyped))
        def wxxw = Vec4(Field("wxxw", untyped))
        def wxyx = Vec4(Field("wxyx", untyped))
        def wxyy = Vec4(Field("wxyy", untyped))
        def wxyz = Vec4(Field("wxyz", untyped))
        def wxyw = Vec4(Field("wxyw", untyped))
        def wxzx = Vec4(Field("wxzx", untyped))
        def wxzy = Vec4(Field("wxzy", untyped))
        def wxzz = Vec4(Field("wxzz", untyped))
        def wxzw = Vec4(Field("wxzw", untyped))
        def wxwx = Vec4(Field("wxwx", untyped))
        def wxwy = Vec4(Field("wxwy", untyped))
        def wxwz = Vec4(Field("wxwz", untyped))
        def wxww = Vec4(Field("wxww", untyped))
        def wyxx = Vec4(Field("wyxx", untyped))
        def wyxy = Vec4(Field("wyxy", untyped))
        def wyxz = Vec4(Field("wyxz", untyped))
        def wyxw = Vec4(Field("wyxw", untyped))
        def wyyx = Vec4(Field("wyyx", untyped))
        def wyyy = Vec4(Field("wyyy", untyped))
        def wyyz = Vec4(Field("wyyz", untyped))
        def wyyw = Vec4(Field("wyyw", untyped))
        def wyzx = Vec4(Field("wyzx", untyped))
        def wyzy = Vec4(Field("wyzy", untyped))
        def wyzz = Vec4(Field("wyzz", untyped))
        def wyzw = Vec4(Field("wyzw", untyped))
        def wywx = Vec4(Field("wywx", untyped))
        def wywy = Vec4(Field("wywy", untyped))
        def wywz = Vec4(Field("wywz", untyped))
        def wyww = Vec4(Field("wyww", untyped))
        def wzxx = Vec4(Field("wzxx", untyped))
        def wzxy = Vec4(Field("wzxy", untyped))
        def wzxz = Vec4(Field("wzxz", untyped))
        def wzxw = Vec4(Field("wzxw", untyped))
        def wzyx = Vec4(Field("wzyx", untyped))
        def wzyy = Vec4(Field("wzyy", untyped))
        def wzyz = Vec4(Field("wzyz", untyped))
        def wzyw = Vec4(Field("wzyw", untyped))
        def wzzx = Vec4(Field("wzzx", untyped))
        def wzzy = Vec4(Field("wzzy", untyped))
        def wzzz = Vec4(Field("wzzz", untyped))
        def wzzw = Vec4(Field("wzzw", untyped))
        def wzwx = Vec4(Field("wzwx", untyped))
        def wzwy = Vec4(Field("wzwy", untyped))
        def wzwz = Vec4(Field("wzwz", untyped))
        def wzww = Vec4(Field("wzww", untyped))
        def wwxx = Vec4(Field("wwxx", untyped))
        def wwxy = Vec4(Field("wwxy", untyped))
        def wwxz = Vec4(Field("wwxz", untyped))
        def wwxw = Vec4(Field("wwxw", untyped))
        def wwyx = Vec4(Field("wwyx", untyped))
        def wwyy = Vec4(Field("wwyy", untyped))
        def wwyz = Vec4(Field("wwyz", untyped))
        def wwyw = Vec4(Field("wwyw", untyped))
        def wwzx = Vec4(Field("wwzx", untyped))
        def wwzy = Vec4(Field("wwzy", untyped))
        def wwzz = Vec4(Field("wwzz", untyped))
        def wwzw = Vec4(Field("wwzw", untyped))
        def wwwx = Vec4(Field("wwwx", untyped))
        def wwwy = Vec4(Field("wwwy", untyped))
        def wwwz = Vec4(Field("wwwz", untyped))
        def wwww = Vec4(Field("wwww", untyped))

        def r = R(Field("r", untyped))
        def g = R(Field("g", untyped))
        def b = R(Field("b", untyped))
        def a = R(Field("a", untyped))

        def rr = Vec2(Field("rr", untyped))
        def rg = Vec2(Field("rg", untyped))
        def rb = Vec2(Field("rb", untyped))
        def ra = Vec2(Field("ra", untyped))
        def gr = Vec2(Field("gr", untyped))
        def gg = Vec2(Field("gg", untyped))
        def gb = Vec2(Field("gb", untyped))
        def ga = Vec2(Field("ga", untyped))
        def br = Vec2(Field("br", untyped))
        def bg = Vec2(Field("bg", untyped))
        def bb = Vec2(Field("bb", untyped))
        def ba = Vec2(Field("ba", untyped))
        def ar = Vec2(Field("ar", untyped))
        def ag = Vec2(Field("ag", untyped))
        def ab = Vec2(Field("ab", untyped))
        def aa = Vec2(Field("aa", untyped))

        def rrr = Vec3(Field("rrr", untyped))
        def rrg = Vec3(Field("rrg", untyped))
        def rrb = Vec3(Field("rrb", untyped))
        def rra = Vec3(Field("rra", untyped))
        def rgr = Vec3(Field("rgr", untyped))
        def rgg = Vec3(Field("rgg", untyped))
        def rgb = Vec3(Field("rgb", untyped))
        def rga = Vec3(Field("rga", untyped))
        def rbr = Vec3(Field("rbr", untyped))
        def rbg = Vec3(Field("rbg", untyped))
        def rbb = Vec3(Field("rbb", untyped))
        def rba = Vec3(Field("rba", untyped))
        def rar = Vec3(Field("rar", untyped))
        def rag = Vec3(Field("rag", untyped))
        def rab = Vec3(Field("rab", untyped))
        def raa = Vec3(Field("raa", untyped))
        def grr = Vec3(Field("grr", untyped))
        def grg = Vec3(Field("grg", untyped))
        def grb = Vec3(Field("grb", untyped))
        def gra = Vec3(Field("gra", untyped))
        def ggr = Vec3(Field("ggr", untyped))
        def ggg = Vec3(Field("ggg", untyped))
        def ggb = Vec3(Field("ggb", untyped))
        def gga = Vec3(Field("gga", untyped))
        def gbr = Vec3(Field("gbr", untyped))
        def gbg = Vec3(Field("gbg", untyped))
        def gbb = Vec3(Field("gbb", untyped))
        def gba = Vec3(Field("gba", untyped))
        def gar = Vec3(Field("gar", untyped))
        def gag = Vec3(Field("gag", untyped))
        def gab = Vec3(Field("gab", untyped))
        def gaa = Vec3(Field("gaa", untyped))
        def brr = Vec3(Field("brr", untyped))
        def brg = Vec3(Field("brg", untyped))
        def brb = Vec3(Field("brb", untyped))
        def bra = Vec3(Field("bra", untyped))
        def bgr = Vec3(Field("bgr", untyped))
        def bgg = Vec3(Field("bgg", untyped))
        def bgb = Vec3(Field("bgb", untyped))
        def bga = Vec3(Field("bga", untyped))
        def bbr = Vec3(Field("bbr", untyped))
        def bbg = Vec3(Field("bbg", untyped))
        def bbb = Vec3(Field("bbb", untyped))
        def bba = Vec3(Field("bba", untyped))
        def bar = Vec3(Field("bar", untyped))
        def bag = Vec3(Field("bag", untyped))
        def bab = Vec3(Field("bab", untyped))
        def baa = Vec3(Field("baa", untyped))
        def arr = Vec3(Field("arr", untyped))
        def arg = Vec3(Field("arg", untyped))
        def arb = Vec3(Field("arb", untyped))
        def ara = Vec3(Field("ara", untyped))
        def agr = Vec3(Field("agr", untyped))
        def agg = Vec3(Field("agg", untyped))
        def agb = Vec3(Field("agb", untyped))
        def aga = Vec3(Field("aga", untyped))
        def abr = Vec3(Field("abr", untyped))
        def abg = Vec3(Field("abg", untyped))
        def abb = Vec3(Field("abb", untyped))
        def aba = Vec3(Field("aba", untyped))
        def aar = Vec3(Field("aar", untyped))
        def aag = Vec3(Field("aag", untyped))
        def aab = Vec3(Field("aab", untyped))
        def aaa = Vec3(Field("aaa", untyped))

        def rrrr = Vec4(Field("rrrr", untyped))
        def rrrg = Vec4(Field("rrrg", untyped))
        def rrrb = Vec4(Field("rrrb", untyped))
        def rrra = Vec4(Field("rrra", untyped))
        def rrgr = Vec4(Field("rrgr", untyped))
        def rrgg = Vec4(Field("rrgg", untyped))
        def rrgb = Vec4(Field("rrgb", untyped))
        def rrga = Vec4(Field("rrga", untyped))
        def rrbr = Vec4(Field("rrbr", untyped))
        def rrbg = Vec4(Field("rrbg", untyped))
        def rrbb = Vec4(Field("rrbb", untyped))
        def rrba = Vec4(Field("rrba", untyped))
        def rrar = Vec4(Field("rrar", untyped))
        def rrag = Vec4(Field("rrag", untyped))
        def rrab = Vec4(Field("rrab", untyped))
        def rraa = Vec4(Field("rraa", untyped))
        def rgrr = Vec4(Field("rgrr", untyped))
        def rgrg = Vec4(Field("rgrg", untyped))
        def rgrb = Vec4(Field("rgrb", untyped))
        def rgra = Vec4(Field("rgra", untyped))
        def rggr = Vec4(Field("rggr", untyped))
        def rggg = Vec4(Field("rggg", untyped))
        def rggb = Vec4(Field("rggb", untyped))
        def rgga = Vec4(Field("rgga", untyped))
        def rgbr = Vec4(Field("rgbr", untyped))
        def rgbg = Vec4(Field("rgbg", untyped))
        def rgbb = Vec4(Field("rgbb", untyped))
        def rgba = Vec4(Field("rgba", untyped))
        def rgar = Vec4(Field("rgar", untyped))
        def rgag = Vec4(Field("rgag", untyped))
        def rgab = Vec4(Field("rgab", untyped))
        def rgaa = Vec4(Field("rgaa", untyped))
        def rbrr = Vec4(Field("rbrr", untyped))
        def rbrg = Vec4(Field("rbrg", untyped))
        def rbrb = Vec4(Field("rbrb", untyped))
        def rbra = Vec4(Field("rbra", untyped))
        def rbgr = Vec4(Field("rbgr", untyped))
        def rbgg = Vec4(Field("rbgg", untyped))
        def rbgb = Vec4(Field("rbgb", untyped))
        def rbga = Vec4(Field("rbga", untyped))
        def rbbr = Vec4(Field("rbbr", untyped))
        def rbbg = Vec4(Field("rbbg", untyped))
        def rbbb = Vec4(Field("rbbb", untyped))
        def rbba = Vec4(Field("rbba", untyped))
        def rbar = Vec4(Field("rbar", untyped))
        def rbag = Vec4(Field("rbag", untyped))
        def rbab = Vec4(Field("rbab", untyped))
        def rbaa = Vec4(Field("rbaa", untyped))
        def rarr = Vec4(Field("rarr", untyped))
        def rarg = Vec4(Field("rarg", untyped))
        def rarb = Vec4(Field("rarb", untyped))
        def rara = Vec4(Field("rara", untyped))
        def ragr = Vec4(Field("ragr", untyped))
        def ragg = Vec4(Field("ragg", untyped))
        def ragb = Vec4(Field("ragb", untyped))
        def raga = Vec4(Field("raga", untyped))
        def rabr = Vec4(Field("rabr", untyped))
        def rabg = Vec4(Field("rabg", untyped))
        def rabb = Vec4(Field("rabb", untyped))
        def raba = Vec4(Field("raba", untyped))
        def raar = Vec4(Field("raar", untyped))
        def raag = Vec4(Field("raag", untyped))
        def raab = Vec4(Field("raab", untyped))
        def raaa = Vec4(Field("raaa", untyped))
        def grrr = Vec4(Field("grrr", untyped))
        def grrg = Vec4(Field("grrg", untyped))
        def grrb = Vec4(Field("grrb", untyped))
        def grra = Vec4(Field("grra", untyped))
        def grgr = Vec4(Field("grgr", untyped))
        def grgg = Vec4(Field("grgg", untyped))
        def grgb = Vec4(Field("grgb", untyped))
        def grga = Vec4(Field("grga", untyped))
        def grbr = Vec4(Field("grbr", untyped))
        def grbg = Vec4(Field("grbg", untyped))
        def grbb = Vec4(Field("grbb", untyped))
        def grba = Vec4(Field("grba", untyped))
        def grar = Vec4(Field("grar", untyped))
        def grag = Vec4(Field("grag", untyped))
        def grab = Vec4(Field("grab", untyped))
        def graa = Vec4(Field("graa", untyped))
        def ggrr = Vec4(Field("ggrr", untyped))
        def ggrg = Vec4(Field("ggrg", untyped))
        def ggrb = Vec4(Field("ggrb", untyped))
        def ggra = Vec4(Field("ggra", untyped))
        def gggr = Vec4(Field("gggr", untyped))
        def gggg = Vec4(Field("gggg", untyped))
        def gggb = Vec4(Field("gggb", untyped))
        def ggga = Vec4(Field("ggga", untyped))
        def ggbr = Vec4(Field("ggbr", untyped))
        def ggbg = Vec4(Field("ggbg", untyped))
        def ggbb = Vec4(Field("ggbb", untyped))
        def ggba = Vec4(Field("ggba", untyped))
        def ggar = Vec4(Field("ggar", untyped))
        def ggag = Vec4(Field("ggag", untyped))
        def ggab = Vec4(Field("ggab", untyped))
        def ggaa = Vec4(Field("ggaa", untyped))
        def gbrr = Vec4(Field("gbrr", untyped))
        def gbrg = Vec4(Field("gbrg", untyped))
        def gbrb = Vec4(Field("gbrb", untyped))
        def gbra = Vec4(Field("gbra", untyped))
        def gbgr = Vec4(Field("gbgr", untyped))
        def gbgg = Vec4(Field("gbgg", untyped))
        def gbgb = Vec4(Field("gbgb", untyped))
        def gbga = Vec4(Field("gbga", untyped))
        def gbbr = Vec4(Field("gbbr", untyped))
        def gbbg = Vec4(Field("gbbg", untyped))
        def gbbb = Vec4(Field("gbbb", untyped))
        def gbba = Vec4(Field("gbba", untyped))
        def gbar = Vec4(Field("gbar", untyped))
        def gbag = Vec4(Field("gbag", untyped))
        def gbab = Vec4(Field("gbab", untyped))
        def gbaa = Vec4(Field("gbaa", untyped))
        def garr = Vec4(Field("garr", untyped))
        def garg = Vec4(Field("garg", untyped))
        def garb = Vec4(Field("garb", untyped))
        def gara = Vec4(Field("gara", untyped))
        def gagr = Vec4(Field("gagr", untyped))
        def gagg = Vec4(Field("gagg", untyped))
        def gagb = Vec4(Field("gagb", untyped))
        def gaga = Vec4(Field("gaga", untyped))
        def gabr = Vec4(Field("gabr", untyped))
        def gabg = Vec4(Field("gabg", untyped))
        def gabb = Vec4(Field("gabb", untyped))
        def gaba = Vec4(Field("gaba", untyped))
        def gaar = Vec4(Field("gaar", untyped))
        def gaag = Vec4(Field("gaag", untyped))
        def gaab = Vec4(Field("gaab", untyped))
        def gaaa = Vec4(Field("gaaa", untyped))
        def brrr = Vec4(Field("brrr", untyped))
        def brrg = Vec4(Field("brrg", untyped))
        def brrb = Vec4(Field("brrb", untyped))
        def brra = Vec4(Field("brra", untyped))
        def brgr = Vec4(Field("brgr", untyped))
        def brgg = Vec4(Field("brgg", untyped))
        def brgb = Vec4(Field("brgb", untyped))
        def brga = Vec4(Field("brga", untyped))
        def brbr = Vec4(Field("brbr", untyped))
        def brbg = Vec4(Field("brbg", untyped))
        def brbb = Vec4(Field("brbb", untyped))
        def brba = Vec4(Field("brba", untyped))
        def brar = Vec4(Field("brar", untyped))
        def brag = Vec4(Field("brag", untyped))
        def brab = Vec4(Field("brab", untyped))
        def braa = Vec4(Field("braa", untyped))
        def bgrr = Vec4(Field("bgrr", untyped))
        def bgrg = Vec4(Field("bgrg", untyped))
        def bgrb = Vec4(Field("bgrb", untyped))
        def bgra = Vec4(Field("bgra", untyped))
        def bggr = Vec4(Field("bggr", untyped))
        def bggg = Vec4(Field("bggg", untyped))
        def bggb = Vec4(Field("bggb", untyped))
        def bgga = Vec4(Field("bgga", untyped))
        def bgbr = Vec4(Field("bgbr", untyped))
        def bgbg = Vec4(Field("bgbg", untyped))
        def bgbb = Vec4(Field("bgbb", untyped))
        def bgba = Vec4(Field("bgba", untyped))
        def bgar = Vec4(Field("bgar", untyped))
        def bgag = Vec4(Field("bgag", untyped))
        def bgab = Vec4(Field("bgab", untyped))
        def bgaa = Vec4(Field("bgaa", untyped))
        def bbrr = Vec4(Field("bbrr", untyped))
        def bbrg = Vec4(Field("bbrg", untyped))
        def bbrb = Vec4(Field("bbrb", untyped))
        def bbra = Vec4(Field("bbra", untyped))
        def bbgr = Vec4(Field("bbgr", untyped))
        def bbgg = Vec4(Field("bbgg", untyped))
        def bbgb = Vec4(Field("bbgb", untyped))
        def bbga = Vec4(Field("bbga", untyped))
        def bbbr = Vec4(Field("bbbr", untyped))
        def bbbg = Vec4(Field("bbbg", untyped))
        def bbbb = Vec4(Field("bbbb", untyped))
        def bbba = Vec4(Field("bbba", untyped))
        def bbar = Vec4(Field("bbar", untyped))
        def bbag = Vec4(Field("bbag", untyped))
        def bbab = Vec4(Field("bbab", untyped))
        def bbaa = Vec4(Field("bbaa", untyped))
        def barr = Vec4(Field("barr", untyped))
        def barg = Vec4(Field("barg", untyped))
        def barb = Vec4(Field("barb", untyped))
        def bara = Vec4(Field("bara", untyped))
        def bagr = Vec4(Field("bagr", untyped))
        def bagg = Vec4(Field("bagg", untyped))
        def bagb = Vec4(Field("bagb", untyped))
        def baga = Vec4(Field("baga", untyped))
        def babr = Vec4(Field("babr", untyped))
        def babg = Vec4(Field("babg", untyped))
        def babb = Vec4(Field("babb", untyped))
        def baba = Vec4(Field("baba", untyped))
        def baar = Vec4(Field("baar", untyped))
        def baag = Vec4(Field("baag", untyped))
        def baab = Vec4(Field("baab", untyped))
        def baaa = Vec4(Field("baaa", untyped))
        def arrr = Vec4(Field("arrr", untyped))
        def arrg = Vec4(Field("arrg", untyped))
        def arrb = Vec4(Field("arrb", untyped))
        def arra = Vec4(Field("arra", untyped))
        def argr = Vec4(Field("argr", untyped))
        def argg = Vec4(Field("argg", untyped))
        def argb = Vec4(Field("argb", untyped))
        def arga = Vec4(Field("arga", untyped))
        def arbr = Vec4(Field("arbr", untyped))
        def arbg = Vec4(Field("arbg", untyped))
        def arbb = Vec4(Field("arbb", untyped))
        def arba = Vec4(Field("arba", untyped))
        def arar = Vec4(Field("arar", untyped))
        def arag = Vec4(Field("arag", untyped))
        def arab = Vec4(Field("arab", untyped))
        def araa = Vec4(Field("araa", untyped))
        def agrr = Vec4(Field("agrr", untyped))
        def agrg = Vec4(Field("agrg", untyped))
        def agrb = Vec4(Field("agrb", untyped))
        def agra = Vec4(Field("agra", untyped))
        def aggr = Vec4(Field("aggr", untyped))
        def aggg = Vec4(Field("aggg", untyped))
        def aggb = Vec4(Field("aggb", untyped))
        def agga = Vec4(Field("agga", untyped))
        def agbr = Vec4(Field("agbr", untyped))
        def agbg = Vec4(Field("agbg", untyped))
        def agbb = Vec4(Field("agbb", untyped))
        def agba = Vec4(Field("agba", untyped))
        def agar = Vec4(Field("agar", untyped))
        def agag = Vec4(Field("agag", untyped))
        def agab = Vec4(Field("agab", untyped))
        def agaa = Vec4(Field("agaa", untyped))
        def abrr = Vec4(Field("abrr", untyped))
        def abrg = Vec4(Field("abrg", untyped))
        def abrb = Vec4(Field("abrb", untyped))
        def abra = Vec4(Field("abra", untyped))
        def abgr = Vec4(Field("abgr", untyped))
        def abgg = Vec4(Field("abgg", untyped))
        def abgb = Vec4(Field("abgb", untyped))
        def abga = Vec4(Field("abga", untyped))
        def abbr = Vec4(Field("abbr", untyped))
        def abbg = Vec4(Field("abbg", untyped))
        def abbb = Vec4(Field("abbb", untyped))
        def abba = Vec4(Field("abba", untyped))
        def abar = Vec4(Field("abar", untyped))
        def abag = Vec4(Field("abag", untyped))
        def abab = Vec4(Field("abab", untyped))
        def abaa = Vec4(Field("abaa", untyped))
        def aarr = Vec4(Field("aarr", untyped))
        def aarg = Vec4(Field("aarg", untyped))
        def aarb = Vec4(Field("aarb", untyped))
        def aara = Vec4(Field("aara", untyped))
        def aagr = Vec4(Field("aagr", untyped))
        def aagg = Vec4(Field("aagg", untyped))
        def aagb = Vec4(Field("aagb", untyped))
        def aaga = Vec4(Field("aaga", untyped))
        def aabr = Vec4(Field("aabr", untyped))
        def aabg = Vec4(Field("aabg", untyped))
        def aabb = Vec4(Field("aabb", untyped))
        def aaba = Vec4(Field("aaba", untyped))
        def aaar = Vec4(Field("aaar", untyped))
        def aaag = Vec4(Field("aaag", untyped))
        def aaab = Vec4(Field("aaab", untyped))
        def aaaa = Vec4(Field("aaaa", untyped))        
    }

    case class BVec2(untyped : Untyped) extends Typed[Boolean] {
        type Self = BVec2
        val make = copy _
        val typeName = "bvec2"
    }

    case class BVec3(untyped : Untyped) extends Typed[Boolean] {
        type Self = BVec3
        val make = copy _
        val typeName = "bvec3"
    }

    case class BVec4(untyped : Untyped) extends Typed[Boolean] {
        type Self = BVec4
        val make = copy _
        val typeName = "bvec4"
    }

    case class IVec2(untyped : Untyped) extends Typed[Int] {
        type Self = IVec2
        val make = copy _
        val typeName = "ivec2"
    }

    case class IVec3(untyped : Untyped) extends Typed[Int] {
        type Self = IVec3
        val make = copy _
        val typeName = "ivec3"
    }

    case class IVec4(untyped : Untyped) extends Typed[Int] {
        type Self = IVec4
        val make = copy _
        val typeName = "ivec4"
    }


    object R {
        def apply(r : Double) : R = R(Constant(r))
    }

    object B {
        //def apply(b : Boolean) = R(Constant(b)) // TODO support boolean constants
    }

    object Vec2 {
        def apply(x : R) : Vec2 = Vec2(Call("vec2", List(x.untyped)))
        def apply(x : R, y : R) : Vec2 = Vec2(Call("vec2", List(x.untyped, y.untyped)))
    }

    object Vec3 {
        def apply(x : R) : Vec2 = Vec2(Call("vec3", List(x.untyped)))
        def apply(x : R, y : R, z : R) : Vec3 = Vec3(Call("vec3", List(x.untyped, y.untyped, z.untyped)))
        def apply(xy : Vec2, z : R) : Vec3 = Vec3(Call("vec3", List(xy.untyped, z.untyped)))
        def apply(x : R, yz : Vec2) : Vec3 = Vec3(Call("vec3", List(x.untyped, yz.untyped)))
    }

    object Vec4 {
        def apply(x : R) : Vec2 = Vec2(Call("vec4", List(x.untyped)))
        def apply(x : R, y : R, z : R, w : R) : Vec4 = Vec4(Call("vec4", List(x.untyped, y.untyped, z.untyped, w.untyped)))
        def apply(xy : Vec2, z : R, w : R) : Vec4 = Vec4(Call("vec4", List(xy.untyped, z.untyped, w.untyped)))
        def apply(xy : Vec2, zw : Vec2) : Vec4 = Vec4(Call("vec4", List(xy.untyped, zw.untyped)))
        def apply(x : R, y : R, zw : Vec2) : Vec4 = Vec4(Call("vec4", List(x.untyped, y.untyped, zw.untyped)))
        def apply(xyz : Vec3, w : R) : Vec4 = Vec4(Call("vec4", List(xyz.untyped, w.untyped)))
        def apply(x : R, yzw : Vec3) : Vec4 = Vec4(Call("vec4", List(x.untyped, yzw.untyped)))
    }
}