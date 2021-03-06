package dk.mzw.scalashading.util

import dk.mzw.scalashading.Native._
import dk.mzw.scalashading.Global._
import dk.mzw.scalashading.Language._
import dk.mzw.scalashading.Math._

object Prelude {

    def round(x : R) = floor(x + 0.5)
    def round(x : Vec2) = floor(x + 0.5)
    def round(x : Vec3) = floor(x + 0.5)
    def round(x : Vec4) = floor(x + 0.5)

    private def hsvaToRgba(h : R, s : R, v : R, a : R) : Vec4 = for {
        c <- Vec3(h, s, v)
        k <- Vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0)
        p <- abs(fract(c.xxx + k.xyz) * 6.0 - k.www)
        r <- c.z * mix(k.xxx, clamp(p - k.xxx, 0.0, 1.0), c.y)
    } yield Vec4(r, a)
    val hsva = (hsvaToRgba _).global("hsvaToRgba")

    val rgbaToHsva = wrapNative1[Vec4, Vec4]("""
        vec4 rgbaToHsva(vec4 c) {
            vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
            vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
            vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

            float d = q.x - min(q.w, q.y);
            float e = 1.0e-10;
            return vec4(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x, c.a);
        }
    """)

    private def gaussianP(variance: R, x: R): R =
        1 / sqrt(variance) * sqrt(2 * pi) * exp(-(x * x) / pow(2 * variance, 2))
    val gaussian = (gaussianP _).global("gaussian")

    private def gaussianOneP(variance : R, x : R) : R =
        exp((-x * x) / (4 * variance * variance))
    val gaussianOne = gaussianOneP _ global "gaussianOne"

    val sigmoid = {x : R => 1 / (1 + exp (-x))}.global

    def sigfade(x : R) : R = sigmoid((x - 0.5) * 10)

    def sinOne(x : R) : R = sin(x - pi/2) * 0.5 + 0.5

    def fromPolar(f : R => R => R) (x : R) (y : R) : R = for {
        r <- Vec2(x, y).magnitude
        phi <- atan(y, x)
    } yield {
        f (r) (phi)
    }

    val simplexNoise = wrapNative1[Vec3, R]("""
        //
        // Description : Array and textureless GLSL 2D/3D/4D simplex
        //               noise functions.
        //      Author : Ian McEwan, Ashima Arts.
        //  Maintainer : ijm
        //     Lastmod : 20110822 (ijm)
        //     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
        //               Distributed under the MIT License. See LICENSE file.
        //               https://github.com/ashima/webgl-noise
        //
        vec3 mod289(vec3 x) {
            return x - floor(x * (1.0 / 289.0)) * 289.0;
        }

        vec4 mod289Vec4(vec4 x) {
            return x - floor(x * (1.0 / 289.0)) * 289.0;
        }

        vec4 permute(vec4 x) {
            return mod289Vec4(((x*34.0)+1.0)*x);
        }

        vec4 taylorInvSqrt(vec4 r) {
            return 1.79284291400159 - 0.85373472095314 * r;
        }

        float snoise(vec3 v) {
            const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
            const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

            // First corner
            vec3 i  = floor(v + dot(v, C.yyy) );
            vec3 x0 =   v - i + dot(i, C.xxx) ;

            // Other corners
            vec3 g = step(x0.yzx, x0.xyz);
            vec3 l = 1.0 - g;
            vec3 i1 = min( g.xyz, l.zxy );
            vec3 i2 = max( g.xyz, l.zxy );

            vec3 x1 = x0 - i1 + C.xxx;
            vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
            vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

            // Permutations
            i = mod289(i);
            vec4 p = permute( permute( permute(
                     i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
                   + i.y + vec4(0.0, i1.y, i2.y, 1.0 ))
                   + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

            // Gradients: 7x7 points over a square, mapped onto an octahedron.
            // The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
            float n_ = 0.142857142857; // 1.0/7.0
            vec3  ns = n_ * D.wyz - D.xzx;

            vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

            vec4 x_ = floor(j * ns.z);
            vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

            vec4 x = x_ *ns.x + ns.yyyy;
            vec4 y = y_ *ns.x + ns.yyyy;
            vec4 h = 1.0 - abs(x) - abs(y);

            vec4 b0 = vec4( x.xy, y.xy );
            vec4 b1 = vec4( x.zw, y.zw );

            vec4 s0 = floor(b0)*2.0 + 1.0;
            vec4 s1 = floor(b1)*2.0 + 1.0;
            vec4 sh = -step(h, vec4(0.0));

            vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
            vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

            vec3 p0 = vec3(a0.xy,h.x);
            vec3 p1 = vec3(a0.zw,h.y);
            vec3 p2 = vec3(a1.xy,h.z);
            vec3 p3 = vec3(a1.zw,h.w);

            //Normalise gradients
            vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
            p0 *= norm.x;
            p1 *= norm.y;
            p2 *= norm.z;
            p3 *= norm.w;

            // Mix final noise value
            vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
            m = m * m;
            return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), dot(p2,x2), dot(p3,x3) ) );
        }
    """)
}
