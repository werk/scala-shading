package dk.mzw.accelemation.samples

import dk.mzw.accelemation.Animations._
import dk.mzw.accelemation.Arithmetic._
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Language._

object HidingDevils {

    def apply = hidingDevils

    def hidingDevils : Animation = multiply (chessDevils) (seekingLights)

    def chessDevils : Animation = bendSpaceTime (devilMirror) (scale(0.04, 0.04) (chess))

    def devilMirror : Animation = additions(
        hellHole(1     , 0.2, 0.05, 2.5, 3 , 2.5,  5),
        hellHole(13    , 0.1, 0.02, 3  , 9 , 0.5,  0.1),
        hellHole(133   , 0.4, 0.05, 2.9, 2 , 1  ,  0.5),
        hellHole(1337  , 0.9, 0.04, 3.1, 11, 5  ,  0.9)
    )

    def hellHole(startTime : R, size : R, timeFactor : R, orbitX : R, orbitY : R, spinSpeed : R, rotationSpeed : R) : Animation =
        timeTravel (startTime) (fastForward (timeFactor) (circle(rotationSpeed) (orbit(orbitX, orbitY) (spin(spinSpeed) (scale(size, size) (displacementBall))))))

    def displacementBall : Animation = {t => x => y =>
        vec2(x, y).magnitude bind {d =>
            gaussianOne(0.3, d) bind {intensity =>
                rgba (intensity * x / d, intensity * y / d, 0, 1)
            }
        }
    }

    def light(color : Color) : Animation = multiply (orbit(1, 2)(spin(0.2)(scale(0.5, 0.6)(gaussBall(0.3))))) {t => x => y => color}

    def seekingLights : Animation =
        additions (
            fastForward(0.2) (spin(0.2) (timeTravel(1)    (light (rgba(0.7, 0.2, 0.2, 1))))),
            fastForward(0.3) (spin(0.3) (timeTravel(133)  (light (rgba(0.2, 0.7, 0.2, 1))))),
            fastForward(0.4) (spin(0.4) (timeTravel(1337) (light (rgba(0.2, 0.2, 0.7, 1)))))
        )
}
