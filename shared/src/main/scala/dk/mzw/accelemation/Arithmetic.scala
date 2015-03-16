package dk.mzw.accelemation

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Language.Math._

object Arithmetic {
    def gaussian(variance : R, x : R) : R =
        1 / sqrt(variance) * sqrt(2 * pi) * exp(-(x*x) / pow(2 * variance, 2))
        //     1 / sqrt(variance) * sqrt(2*pi) * exp(-(x*x) / (2 * variance)**2)

    def gaussianOne(variance : R, x : R) : R =
        gaussian(variance, x) / gaussian(variance, 0)

    def sigmoid(x : R) : R = 1 / (1 + exp (-x))


    def sigfade(x : R) : R = sigmoid((x - 0.5) * 10)

    def sinOne(x : R) : R = sin(x - pi/2) * 0.5 + fromDouble(0.5) // TODO

    def fromPolar[A](f : R => R => Term[A]) (x : R) (y : R) : Term[A] = {
        vec2(x, y).magnitude.bind{r =>
            atan2(x, y).bind(phi => f (r) (phi))
        }
    }

    def atan2(x : R, y : R) : R = 2 * atan(y / (vec2(x, y).magnitude + x))
}
