package dk.mzw.scalashading.samples

import dk.mzw.scalashading.util.Combinators._
import dk.mzw.scalashading.Math._
import dk.mzw.scalashading.Language._
import dk.mzw.scalashading.Global._
import dk.mzw.scalashading.util.Prelude._

object TimeLens {

    val gaussBall : R => Animation = {variance : R => t : Time => x : R => y : R =>
        for {
            d <- Vec2(x, y).magnitude
            intensity <- gaussianOne(variance, d)
        } yield rgba(intensity, intensity, intensity, 1)
    }.global("gaussBall")

    val rainbow : Animation = {t => x => y => hsva(mod(x, 1), 0.5, 0.5, 1)}

    val wave : Animation = { t => x => y =>
        (1 - abs(y - sin(x))) bind { intensity =>
            rgba(intensity, intensity, intensity, 1)
        }
    }

    val spinBall : Animation = circle (1) (gaussBall(0.3))

    val balls : Animation = addition (addition (spinBall) (timeTravel (2*pi/3) (spinBall))) (timeTravel (4*pi/3) (spinBall))

    val colorbow : Animation = scroll (1, 0) (multiply (scale (10, 1) (rainbow)) (wave))

    val rainballs : Animation = addition (colorbow) (balls)

    val hole : Animation = multiply (circle (0.7) (gaussBall(0.3))) ({t => x => y => rgba(0, 0, 5, 1)})

    val timebow : Animation = bendSpaceTime (hole) (scroll (1,0) (rainbow))

    val timeballs : Animation = bendSpaceTime (hole) (rainballs)

    val apply = timeballs

}
