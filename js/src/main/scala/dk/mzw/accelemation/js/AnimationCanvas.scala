package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.Animation
import dk.mzw.accelemation.ToGlsl
import org.scalajs.dom
import org.scalajs.dom.raw.{MouseEvent, Element}

class AnimationCanvas(animation : Animation) {

    private val (glsl, uniforms) = ToGlsl.withUniforms(animation)
    //println(glsl)
    val canvas = dom.document.createElement("canvas")
    private val animade = new Animade(Animade.Configuration(glsl, canvas))

    def update(t : Double): Unit = {
        animade.resize(dom.window.innerWidth, dom.window.innerHeight) // TODO container size
        val uniformMap = uniforms.map{u => u.name -> (u.value match {
                case d : Double => List(d)
                case (d1 : Double, d2 : Double) => List(d1, d2)
                case (d1 : Double, d2 : Double, d3 : Double) => List(d1, d2, d3)
                case (d1 : Double, d2 : Double, d3 : Double, d4 : Double) => List(d1, d2, d3, d4)
            })}.toMap
        animade.set(uniformMap)
        animade.draw(Map("u_time" -> List[Double](t)))
    }

    def pixelToUnit(pixelX : Double, pixelY : Double) : (Double, Double) = {
        val resolutionX = dom.window.innerWidth.toDouble // TODO container size, offset, rtc
        val resolutionY = dom.window.innerHeight.toDouble
        val unitX = pixelX / resolutionX * 2.0 - 1
        val unitY = pixelY / resolutionY * 2.0 - 1
        val aspectX = scala.math.max(resolutionX / resolutionY, 1.0)
        val aspectY = scala.math.max(resolutionY / resolutionX, 1.0)
        (unitX * aspectX, -unitY * aspectY)
    }

    def mouse = pixelToUnit(AnimationCanvas.cursorX, AnimationCanvas.cursorY)
}

object AnimationCanvas {
    private var cursorX : Double = 0
    private var cursorY : Double = 0
    dom.document.onmousemove = {e : MouseEvent => {
        cursorX = e.pageX
        cursorY = e.pageY
    }}
}
