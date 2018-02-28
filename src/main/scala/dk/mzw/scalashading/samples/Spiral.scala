package dk.mzw.scalashading.samples

import dk.mzw.scalashading.util.Combinators._
import dk.mzw.scalashading.Math.{floor, pi}
import dk.mzw.scalashading.Language._
import dk.mzw.scalashading.util.Prelude._

object Spiral {

    def apply = translate(0.4, 0.21) (fromPolarCoordinates(spiral))

    //def zoomy(f : Animation) (t : R) (x : R) (y : R) : Color = scaleUniform (sinOne(t)) (f) (t) (x) (y)

    def spiral : Animation = { t => r => phi => for {
        positivePhi <- if_(phi < 0, 2 * pi + phi, phi)
        unitAngle <- positivePhi / (2 * pi)
        d <- floor(r + unitAngle) - unitAngle + 0.5
        h <- d / 77
        s <- 0.5
        v <- gaussianOne(0.03, r - d)
    } yield {
        hsva(h, s, v, 1)
    }}


}
