package dk.mzw.accelemation

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.Language.Math._
import dk.mzw.accelemation.Arithmetic._

object Animations {

    val chess : Animation = {t => x => y =>
        floor(mod(x, 2)) bind { intensityX =>
            floor(mod(y, 2)) bind { intensityY =>
                abs(intensityY - intensityX) bind { intensity =>
                    rgba(intensity, intensity, intensity, 1)
                }
            }
        }
    }

    val gaussBall : R => Animation = {variance => t => x => y =>
        vec2(x, y) bind {
            _.magnitude bind { d =>
                gaussianOne(variance, d) bind { intensity =>
                    rgba(intensity, intensity, intensity, 1)
                }
            }
        }
    }

    def coordinateSystem : Animation = {t => x => y =>
        val variance = 0.005
        val xTick = gaussianOne(variance, x - Math.round(x))
        val yTick = gaussianOne(variance, y - Math.round(y))
        val axis = gaussianOne(variance, x) + gaussianOne(variance, y)
        rgba(xTick + yTick, xTick + yTick, axis, 1)
    }

}
