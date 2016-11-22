package dk.mzw.accelemation

import dk.mzw.accelemation.NewGlobal._
import dk.mzw.accelemation.External._
import dk.mzw.accelemation.BuildInFunctions._

object NewArithmetic {
    val pi = 3.14 // todo

    private def gaussianP(variance: R, x: R): R =
        1 / sqrt(variance) * sqrt(2 * pi) * exp(-(x * x) / pow(2 * variance, 2))
    val gaussian = (gaussianP _).global("gaussian")

    private def gaussianOneP(variance : R, x : R) : R =
        exp((-x * x) / (4 * variance * variance))
    val gaussianOne = gaussianOneP _ global "gaussianOne"

    val sigmoid = {x : R => 1 / (1 + exp (-x))}.global

    def sigfade(x : R) : R = sigmoid((x - 0.5) * 10)

    def sinOne(x : R) : R = sin(x - pi/2) * 0.5 + 0.5

    def fromPolar[T <: Typed[T]](f : R => R => T) (x : R) (y : R) : T = {
        Vec2(x, y).magnitude.bind{r =>
            atan2(x, y).bind(phi => f (r) (phi))
        }
    }

    def atan2(x : R, y : R) : R = 2 * atan(y, Vec2(x, y).magnitude + x)
}
