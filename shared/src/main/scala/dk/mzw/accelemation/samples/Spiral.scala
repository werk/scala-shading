package dk.mzw.accelemation.samples

import dk.mzw.accelemation.Arithmetic._
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.BuildInFunctions.{pi, floor}
import dk.mzw.accelemation.External._
import dk.mzw.accelemation.Prelude._

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
