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

    def fastForward(factor : R) (animation : Animation) : Animation = t => x => y => animation(t * factor)(x)(y)

    def spiral : Animation = { t => r => phi =>
        if_(phi < 0, 2 * pi + phi, phi) bind { positivePhi =>
            positivePhi / (2 * pi) bind { unitAngle =>
                val d = floor(r + unitAngle) - unitAngle + fromDouble(0.5)
                val v = gaussianOne(0.03, r - d)
                hsva(v, v, v, 1)
            }
        }
    }


    def squareTiling(factor : R) (animation : Animation) : Animation = { t => x => y =>
        animation(t) (Math.mod(x, factor * 0.5) * factor * 10 - 1) (Math.mod(y, factor * 0.5) * factor * 10 - 1)
    }

    def fishEye(factor : R)(animation : Animation) : Animation = t => x => y => {
        Math.pow(vec2(x, y).magnitude, factor).bind{d =>
            animation(t) (x * d) (y * d)
        }
    }

    def zoom (factor : R) (animation : Animation) : Animation = t => x => y => animation(t) (x * factor) (y * factor)


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

    val effects = List[(String, R => Animation => Animation)](
        "Spin" -> Combinators.spin _,
        "Circle" -> Combinators.circle _,
        "Jump" -> timeTravel _,
        "Fast forward" -> fastForward _,
        "Squares" -> squareTiling _,
        "Fish eye" -> fishEye _,
        "Zoom" -> zoom _,
        "Scroll horizontal" -> {f => scroll((f - 0.5) * 5, 0)},
        "Scroll vertical" -> {f => scroll(0, (f - 0.5) * 5)},
        "Move horizontal" -> {f => Combinators.translate ((f - 0.5) * 5, 0)},
        "Move vertical" -> {f => Combinators.translate (0, (f - 0.5) * 5)}
    )

    val combinators = List[(String, Animation => Animation => Animation)](
        "Transform" -> Combinators.bendSpaceTime,
        "Add" -> Combinators.addition,
        "Subtract" -> Combinators.subtract,
        "Multiply" -> Combinators.multiply
    )



}
