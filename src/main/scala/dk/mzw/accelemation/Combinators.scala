package dk.mzw.accelemation

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Language.Math._
import dk.mzw.accelemation.Arithmetic.atan2

object Combinators {

    def translate(dx : R, dy : R) (animation : Animation) (t : R) (x : R) (y : R) =
         animation (t) (dx + x) (dy + y)

    def scale(scaleX : R, scaleY : R) (animation : Animation) (t : R) (x : R) (y : R) =
        animation (t) (x / scaleX) (y / scaleY)

    def scaleUniform(factor : R) (animation : Animation) (t : R) (x : R) (y : R) = 
        scale(factor, factor) _

    def rotate(angle : R) (animation : Animation) (t : R) (x : R) (y : R) =
        animation (t) (x * cos(angle) - y * sin(angle)) (x * sin(angle) + y * cos(angle))

    def scroll(speedX : R, speedY : R) (animation : Animation) (t : R) =
        translate(speedX * t, speedY * t) (animation) (t) _

    def circle(speed : R) = orbit(speed, speed) _

    def orbit(speedX : R, speedY : R) (animation : Animation) (t : R) =
        translate(cos(speedX * t), sin(speedY * t)) (animation) (t) _

    def spin(speed : R) (animation : Animation) (t : R) =
        rotate (speed * t) (animation) (t) _

    def timeTravel (dt : R) (animation : Animation) (t : R) (x : R) (y : R) = 
        animation (t + dt) (x) (y)

    def fastForward (speed : R) (animation : Animation) (t : R) (x : R) (y : R) =
        animation (t * speed) (x) (y)

    def bendSpaceTime (f : Animation) (target : Animation) (t : R) (x : R) (y : R) = {
        f(t)(x)(y) bind { spaceTimeColor =>
            spaceTimeColor.red bind { dx =>
                spaceTimeColor.green bind { dy =>
                    spaceTimeColor.blue bind { dt =>
                        spaceTimeColor.alpha bind { a =>
                            target(t + dt * a)(x + dx * a)(y + dy * a)
                        }
                    }
                }
            }
        }
    }

        //fromPolarCoordinates :: Animation -> Animation
    def fromPolarCoordinates (f : Animation) (t : R) (x : R) (y : R) : Color =
        sqrt (x**2 + y**2) bind { r =>
            atan2(x, y) bind { phi =>
                f (t) (r) (phi)
            }
        }

    /*******************************
    ** Animation blendings
    *******************************/

    def liftA2(f : Image => Image => Image) : Animation => Animation => Animation = {
        {a1 : Animation => a2 : Animation =>
            t : Time => f (a1 (t)) (a2 (t))
        }
    }

    //multiply :: Animation -> Animation -> Animation
    def multiply = liftA2 (multiplyImage)

    //screen :: Animation -> Animation -> Animation
    def screen = liftA2 (screenImage)

    //addition :: Animation -> Animation -> Animation
    def addition = liftA2 (additionImage)

    //subtract :: Animation -> Animation -> Animation
    def subtract = liftA2 (subtractImage)

    //top :: Animation -> Animation -> Animation
    def top = liftA2 (topImage)


    /*********************************
    ** Image blendings
    *********************************/

    def blender(binOp : (R, R) => R) (f : Image) (g : Image) (x : R) (y : R) =
        f (x) (y) bind { c1 =>
            g (x) (y) bind { c2 =>
                rgba(
                    binOp(c1.red, c2.red),
                    binOp(c1.green, c2.green),
                    binOp(c1.blue, c2.blue),
                    (c1.alpha + c2.alpha) / 2
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
                def combine(component: Color => R) = component(a) * a.alpha + component(b) * b.alpha * (1 - min(1, a.alpha))
                rgba(
                    combine(_.red),
                    combine(_.green),
                    combine(_.blue),
                    a.alpha + b.alpha * (1 - min(1, a.alpha))
                )
            }
        }
}
