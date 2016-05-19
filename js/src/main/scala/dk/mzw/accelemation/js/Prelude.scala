package dk.mzw.accelemation.js

import dk.mzw.accelemation.Animations._
import dk.mzw.accelemation.Arithmetic._
import dk.mzw.accelemation.Combinators
import dk.mzw.accelemation.Combinators._
import dk.mzw.accelemation.Language.Math._
import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.samples.TimeLens

object Prelude {

    def binary(animation: Time => R => R => B): Animation = { t => x => y =>
        if_(animation(t)(x)(y), 1, 0) bind {intensity =>
            rgba(intensity, intensity, intensity, 1)
        }
    }

    val noise: Animation = t => x => y => simplexNoise(x, y, t) bind {i => rgba(i, i, i, 1)}

    val ball: Animation = binary { t => x => y =>
        sqrt(x * x + y * y) < 1
    }

    val circle: Animation = binary { t => x => y =>
        sqrt(x * x + y * y).bind{ distance =>
            distance > 0.5 && distance < 1
        }
    }

    val square: Animation = binary { t => x => y =>
        abs(x) < 0.5 && abs(y) < 0.5
    }

    def blur(animation: Animation): Animation = { t => x => y =>
        val e = 0.01: R
        val ur = animation(t)(x + e)(y + e)
        val ul = animation(t)(x - e)(y + e)
        val ll = animation(t)(x - e)(y - e)
        val lr = animation(t)(x + e)(y - e)
        val red = (ur.red + ul.red + ll.red + lr.red) / 4
        val green = (ur.green + ul.green + ll.green + lr.green) / 4
        val blue = (ur.blue + ul.blue + ll.blue + lr.blue) / 4
        val alpha = (ur.alpha + ul.alpha + ll.alpha + lr.alpha) / 4
        rgba(red, green, blue, alpha)
    }

    val rainbow: Animation = { t => x => y => hsva(mod(x, 1), 0.5, 0.5, 1) }

    val wave: Animation = { t => x => y =>
        (1 - abs(y - sin(x))) bind { intensity =>
            rgba(intensity, intensity, intensity, 1)
        }
    }


    val flower: Animation = { t => r => phi =>
        val n = Math.floor(t / (2 * pi) + Math.mod(1, 10))
        val d = sinOne(t + phi * n) * sinOne(t)
        val h = t / 77
        val s = d
        val v = gaussianOne(0.05, r - d)
        hsva(h, s, v, 1)
    }

    val flowers = addition(addition(flower)(timeTravel(133)(flower)))(timeTravel(13)(flower))


    def cartesianToPolar(x: R, y: R): (R, R) = (vec2(x, y).magnitude, Math.atan2(y, x))
    def polarToCartesian(r: R, phi: R): (R, R) = (r * Math.cos(phi), r * Math.sin(phi))


    def fromPolar(f: Animation): Animation = { t => x => y =>
        val (r, phi) = cartesianToPolar(x, y)
        f(t)(r)(phi)
    }

    def toPolar(f: Animation): Animation = { t => r => phi =>
        val (x, y) = polarToCartesian(r, phi)
        f(t)(x)(y)
    }

    def timeTunnel(timeRadius : R => R)(factor: R)(animation: Animation): Animation = t => x => y => {
        val (r, phi) = cartesianToPolar(x, y)
        phi.bind { phi => ((-t) + timeRadius(r)).bind {
            newT => toPolar(animation)(newT)(0.5 + factor)(phi)
        }
        }

    }


    def fastForward(factor: R)(animation: Animation): Animation = t => x => y => animation(t * factor)(x)(y)

    def spiral: Animation = { t => r => phi =>
        if_(phi < 0, 2 * pi + phi, phi) bind { positivePhi =>
            positivePhi / (2 * pi) bind { unitAngle =>
                val d = floor(r + unitAngle) - unitAngle + fromDouble(0.5)
                val v = gaussianOne(0.03, r - d)
                hsva(v, v, v, 1)
            }
        }
    }


    def squareTiling(factor: R)(animation: Animation): Animation = { t => x => y =>
        animation(t)(Math.mod(x, factor * 0.5) * factor * 10 - 1)(Math.mod(y, factor * 0.5) * factor * 10 - 1)
    }

    def triangleTiling(factor: R)(f: R => R => Animation): Animation = { t => x => y =>
        def fromRotatedYCoordinates(angle: R, x: R, y: R): (R, R) = (x + y * cos(0.5 * pi + angle), y * sin(0.5 * pi + angle))
        def toRotatedYCoordinates(angle: R, x: R, y: R): (R, R) = (x - y * cos(0.5 * pi + angle), y / sin(0.5 * pi + angle))

        def scale(scaleX: R)(scaleY: R)(animation: Animation): Animation = { t => x => y =>
            animation(t)(x / scaleX)(y / scaleY)
        }

        def scaleUniform(factor: R): Animation => Animation = scale(factor)(factor)

        val (x1, y1) = toRotatedYCoordinates(-pi / 6, x, y)
        x1.bind { x1 => y1.bind { y1 =>
            val centerX1 = Math.round(x1)
            val centerY1 = Math.round(y1)
            val down = {
                val dx1 = Math.mod(x1 - 0.5, 1)
                val dy1 = Math.mod(y1 - 0.5, 1)
                Math.floor(dx1 + dy1)
            }
            down.bind { down =>
                val px = centerX1
                val py = centerY1 * 2 + down
                val q = (down * 2 - 1) * 0.152 // TODO find the right constant
            val (x2, y2) = fromRotatedYCoordinates(-pi / 6, (centerX1 + q) - x1, (centerY1 + q) - y1)
                x2.bind { x2 => y2.bind { y2 =>
                    val animation: Animation = f(px)(py)
                    scaleUniform(0.25)(animation)(t)(x2)(y2)
                }
                }
            }
        }
        }

    }


    def fishEye(factor: R)(animation: Animation): Animation = t => x => y => {
        Math.pow(vec2(x, y).magnitude, 0.5 + factor).bind { d =>
            animation(t)(x * d)(y * d)
        }
    }

    def bendSpaceTimeHsv (target : Animation) = bendSpaceTime(liftColor(rgbaToHsva)(target)) _

    def zoom(factor: R)(animation: Animation): Animation = t => x => y => animation(t)(x * factor)(y * factor)

    def fromFactor(f: R): R = (f - 0.5) * 5

    def limitResolution(factor : R)(v : R) : R = {
        val resolution = factor * 10
        (Math.round(v * resolution) / resolution) bind (x => x)
    }

    def limitColorRecolustion(factor: R)(animation: Animation): Animation = {
        def lessColors(factor: R)(color: Color): Color = {
            val round = limitResolution(factor) _
            rgba(round(color.red), round(color.green), round(color.blue), color.alpha)
        }
        liftColor(lessColors(factor))(animation)
    }

    def limitSpatialResolution(factor: R)(animation: Animation): Animation = { t => x => y =>
        val round = limitResolution(factor) _
        animation(t) (round(x)) (round(y))
    }

    def limitTimeResolution(factor: R)(animation: Animation): Animation = { t => x => y =>
        val round = limitResolution(factor) _
        animation(round(t))(x) (y)
    }

}
