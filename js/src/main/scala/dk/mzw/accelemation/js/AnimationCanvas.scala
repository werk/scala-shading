package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.Animation
import dk.mzw.accelemation.ToGlsl
import org.scalajs.dom
import org.scalajs.dom.raw.Element

object AnimationCanvas {

    def apply(animation : Animation) : (Element, Double => Unit) = {

        val (glsl, uniforms) = ToGlsl.withUniforms(animation)
        println(glsl)
        val canvas = dom.document.createElement("canvas")
        val animade = new Animade(Animade.Configuration(glsl, canvas))

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

        (canvas, update)
    }

}
