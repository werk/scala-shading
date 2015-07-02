package dk.mzw.accelemation

import dk.mzw.accelemation.Arithmetic._
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Language.Math._

object Spiral {

    def apply = translate(0.4, 0.21) (fromPolarCoordinates(spiral)) _

    //def zoomy(f : Animation) (t : R) (x : R) (y : R) : Color = scaleUniform (sinOne(t)) (f) (t) (x) (y)

    def spiral : Animation = { t => r => phi =>
        if_(phi < 0, 2 * pi + phi, phi) bind { positivePhi =>
            positivePhi / (2 * pi) bind { unitAngle =>
                val d = floor(r + unitAngle) - unitAngle + fromDouble(0.5)
                val h = d / 77
                val s = 0.5
                val v = gaussianOne(0.03, r - d)
                hsva(h, s, v, 1)
            }
        }
    }


}
