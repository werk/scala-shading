package dk.mzw.accelemation

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Language.Math._
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Animations._

object TimeLens {

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
