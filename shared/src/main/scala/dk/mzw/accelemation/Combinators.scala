package dk.mzw.accelemation

import dk.mzw.accelemation.External._
import dk.mzw.accelemation.BuildInFunctions._
import dk.mzw.accelemation.Arithmetic.atan2

object Combinators {

    def liftColor(f : Color => Color) (animation : Animation) : Animation = t => x => y => f(animation(t) (x) (y))

    def translate(dx : R, dy : R) (animation : Animation) : Animation  = t => x => y =>
        (dx - x).bind{x2 => (dy - y).bind{y2 => animation (t) (x2) (y2)}}

    def scale(scaleX : R, scaleY : R) (animation : Animation) (t : R) (x : R) (y : R) =
        animation (t) (x / scaleX) (y / scaleY)

    def scaleUniform(factor : R) (animation : Animation) (t : R) (x : R) (y : R) =
        scale(factor, factor) _

    def rotate(angle : R) (animation : Animation) (t : R) (x : R) (y : R) =
        (x * cos(angle) - y * sin(angle)).bind { newX =>
            (x * sin(angle) + y * cos(angle)).bind{ newY =>
                animation (t) (newX) (newY)
            }
        }

    def scroll(speedX : R, speedY : R) (animation : Animation) (t : R) =
        translate(speedX * t, speedY * t) (animation) (t)

    def circle(speed : R) = orbit(speed, speed) _

    def orbit(speedX : R, speedY : R) (animation : Animation) (t : R) =
        translate(cos(speedX * t), sin(speedY * t)) (animation) (t)

    def spin(speed : R) (animation : Animation) (t : R) =
        rotate (speed * t) (animation) (t) _

    def timeTravel (dt : R) (animation : Animation) (t : R) (x : R) (y : R) = 
        animation (t + dt) (x) (y)

    def fastForward (speed : R) (animation : Animation) : Animation = { t => x => y =>
        animation(t * speed)(x)(y)
    }

    def bendSpaceTime (f : Animation) (target : Animation) (t : R) (x : R) (y : R) = for {
        spaceTimeColor <- f(t)(x)(y)
        dx <- spaceTimeColor.r
        dy <- spaceTimeColor.g
        dt <- spaceTimeColor.b
        a <- spaceTimeColor.a
    } yield target(t + dt * a)(x + dx * a)(y + dy * a)

    //fromPolarCoordinates :: Animation -> Animation
    def fromPolarCoordinates (f : Animation) (t : R) (x : R) (y : R) : Color = for{
        r <- vec2(x, y).magnitude
        phi <- atan2(x, y)
    } yield f (t) (r) (phi)

    def colorMap (f : Animation) (target : Animation) (t : R) (x : R) (y : R) : Color =
        f(t)(x)(y) bind { c =>
            (0.2126 * c.r + 0.7152 * c.g + 0.0722 * c.b) bind { l =>
                target(t)(l * 2 - 1)(0)
            }
        }

    /*******************************
    ** Animation blendings
    *******************************/

    def liftImage2(f : Image => Image => Image) : Animation => Animation => Animation = {
        {a1 : Animation => a2 : Animation =>
            t : Time => f (a1 (t)) (a2 (t))
        }
    }

    val multiply = liftImage2 (multiplyImage)
    val screen = liftImage2 (screenImage)
    val addition = liftImage2 (additionImage)
    val subtract = liftImage2 (subtractImage)
    val top = liftImage2 (topImage)
    
    def multiplications(animations : Animation*) : Animation = {
        animations.tail.fold (animations.head) {case (a, b) => multiply (a) (b)}  
    }

    def screens(animations : Animation*) : Animation = {
        animations.tail.fold (animations.head) {case (a, b) => screen (a) (b)}
    }

    def additions(animations : Animation*) : Animation = {
        animations.tail.fold (animations.head) {case (a, b) => addition (a) (b)}
    }

    def subtractions(animations : Animation*) : Animation = {
        animations.tail.fold (animations.head) {case (a, b) => subtract (a) (b)}
    }

    def tops(animations : Animation*) : Animation = {
        animations.tail.fold (animations.head) {case (a, b) => top (a) (b)}
    }

    /*********************************
    ** Image blendings
    *********************************/

    def blender(binOp : (R, R) => R) (f : Image) (g : Image) (x : R) (y : R) =
        f (x) (y) bind { c1 =>
            g (x) (y) bind { c2 =>
                rgba(
                    binOp(c1.r, c2.r),
                    binOp(c1.g, c2.g),
                    binOp(c1.b, c2.b),
                    (c1.a + c2.a) / 2
                )
            }
        }

    def multiplyImage = blender {case (a, b) => a * b} _

    def screenImage = blender {case (a, b) => 1 - (1 - a) * (1 - b)} _

    def additionImage = blender {case (a, b) => min(1, a + b)} _

    def subtractImage = blender {case (a, b) => max(0, a - b)} _

    def topImage (f : Image) (g : Image) (x : R) (y : R) =
        f(x)(y) bind { a: Color =>
            g(x)(y) bind { b: Color =>
                def combine(component: Color => R) = component(a) * a.a + component(b) * b.a * (1 - min(1, a.a))
                rgba(
                    combine(_.r),
                    combine(_.g),
                    combine(_.b),
                    a.a + b.a * (1 - min(1, a.a))
                )
            }
        }
}
