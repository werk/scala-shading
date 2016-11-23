package dk.mzw.accelemation

import dk.mzw.accelemation.External._
import dk.mzw.accelemation.BuildInFunctions._
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
}
