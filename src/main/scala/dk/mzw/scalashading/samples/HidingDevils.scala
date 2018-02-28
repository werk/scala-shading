package dk.mzw.scalashading.samples

import dk.mzw.scalashading.Global._
import dk.mzw.scalashading.util.Combinators._
import dk.mzw.scalashading.Language._
import dk.mzw.scalashading.Math._
import dk.mzw.scalashading.util.Prelude._

object HidingDevils {

    val chess : Animation = {t : Time => x : R => y : R=>
        for {
            intensityX <- floor(mod(x, 2))
            intensityY <- floor(mod(y, 2))
            intensity <- abs(intensityY - intensityX)
        } yield rgba(intensity, intensity, intensity, 1)
    }.global("chess")

    val gaussBall : R => Animation = {variance : R => t : Time => x : R => y : R =>
        for {
            d <- Vec2(x, y).magnitude
            intensity <- gaussianOne(variance, d)
        } yield rgba(intensity, intensity, intensity, 1)
    }.global("gaussBall")

    def hellHoleP(startTime : R, size : R, timeFactor : R, orbitX : R, orbitY : R, spinSpeed : R, rotationSpeed : R) : Animation =
        timeTravel (startTime) (fastForward (timeFactor) (circle(rotationSpeed) (orbit(orbitX, orbitY) (spin(spinSpeed) (scale(size, size) (displacementBall))))))
    val hellHole = (hellHoleP _).global("hellHole")

    val devilMirror : Animation = additions(
        hellHole(1     , 0.2, 0.05, 2.5, 3 , 2.5,  5),
        hellHole(13    , 0.1, 0.02, 3  , 9 , 0.5,  0.1),
        hellHole(133   , 0.4, 0.05, 2.9, 2 , 1  ,  0.5),
        hellHole(1337  , 0.9, 0.04, 3.1, 11, 5  ,  0.9)
    )

    val chessDevils : Animation = bendSpaceTime (devilMirror) (scale(0.04, 0.04) (chess))

    def displacementBall : Animation = {t => x => y =>
        Vec2(x, y).magnitude bind {d =>
            gaussianOne(0.3, d) bind {intensity =>
                rgba (intensity * x / d, intensity * y / d, 0, 1)
            }
        }
    }

    def light(color : Color) : Animation = multiply (orbit(1, 2)(spin(0.2)(scale(0.5, 0.6)(gaussBall(0.3))))) {t => x => y => color}

    val seekingLight = {(tf : R, s : R, td : R, color : Color) =>
        fastForward(tf) (spin(s) (timeTravel(td)  (light (color))))
    }.global("seekingLight")

    def seekingLights : Animation =
        additions (
            seekingLight(0.2, 0.2, 1, rgba(0.7, 0.2, 0.2, 1)),
            seekingLight(0.3, 0.3, 133, rgba(0.2, 0.7, 0.2, 1)),
            seekingLight(0.4, 0.4, 1337, rgba(0.2, 0.2, 0.7, 1))
            //fastForward(0.2) (spin(0.2) (timeTravel(1)    (light (rgba(0.7, 0.2, 0.2, 1))))),
            //fastForward(0.3) (spin(0.3) (timeTravel(133)  (light (rgba(0.2, 0.7, 0.2, 1))))),
            //fastForward(0.4) (spin(0.4) (timeTravel(1337) (light (rgba(0.2, 0.2, 0.7, 1)))))
        )

    val hidingDevils : Animation = multiply (chessDevils) (seekingLights)
    val apply = hidingDevils

}
