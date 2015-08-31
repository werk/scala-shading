package dk.mzw.accelemation.js

import dk.mzw.accelemation.Animations._
import dk.mzw.accelemation.Arithmetic._
import dk.mzw.accelemation.Combinators
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Language.Math._
import dk.mzw.accelemation.Language._

object GlobalAnimations {

    val rainbow : Animation = {t => x => y => hsva(mod(x, 1), 0.5, 0.5, 1)}

    val wave : Animation = { t => x => y =>
        (1 - abs(y - sin(x))) bind { intensity =>
            rgba(intensity, intensity, intensity, 1)
        }
    }

    def fastForward(animation : Animation) : Animation = t => x => y => animation(t * 2)(x)(y)

    def spiral : Animation = { t => r => phi =>
        if_(phi < 0, 2 * pi + phi, phi) bind { positivePhi =>
            positivePhi / (2 * pi) bind { unitAngle =>
                val d = floor(r + unitAngle) - unitAngle + fromDouble(0.5)
                val v = gaussianOne(0.03, r - d)
                hsva(v, v, v, 1)
            }
        }
    }


    def squareTiling(animation : Animation) : Animation = { t => x => y =>
        animation(t) (Math.mod(x, 0.1) * 10) (Math.mod(y, 0.1) * 10)
    }


    val animations = List[(String, Animation)](
        "Ball" -> gaussBall(0.3),
        "Chess" -> chess,
        "Wave" -> wave,
        "Rainbow" -> rainbow,
        "Red" -> (t => x => y => rgba(1, 0, 0, 1)),
        "Green" -> (t => x => y => rgba(0, 1, 0, 1)),
        "Blue" -> (t => x => y => rgba(0, 0, 1, 1)),
        "Spiral" -> spiral

    )

    val effects = List[(String, Animation => Animation)](
        "Scroll" -> scroll(1, 0),
        "Spin" -> Combinators.spin(2),
        "Circle" -> Combinators.circle(1),
        "Jump" -> timeTravel (1),
        "Fast forward" -> fastForward,
        "Squares" -> squareTiling,

        "Up" -> Combinators.translate (0, 1),
        "Left" -> Combinators.translate (1, 0)

    )

    val combinators = List[(String, Animation => Animation => Animation)](
        "Transform" -> Combinators.bendSpaceTime,
        "Add" -> Combinators.addition,
        "Subtract" -> Combinators.subtract,
        "Multiply" -> Combinators.multiply
    )



}
