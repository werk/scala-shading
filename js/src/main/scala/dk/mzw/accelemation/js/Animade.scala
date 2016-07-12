package dk.mzw.accelemation.js

import dk.mzw.accelemation.js.Animade.{AnimadeJs, Configuration}
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSName, JSExportAll}

class Animade(configuration : Configuration) {
    val animade = new AnimadeJs(configuration)
    def resize(width : Int, height : Int) : Unit = animade.resize(width, height)
    def set(uniforms : Map[String, List[Double]]) : Unit = animade.set(js.Dictionary(uniforms.map { case (k, v) => k -> js.Array(v : _*) }.toSeq : _*))
    def setArray(name : String, array : Array[Double]) : Unit = animade.setArray(name, js.Array(array : _*))
    def draw(uniforms : Map[String, List[Double]]) : Unit = animade.draw(js.Dictionary(uniforms.map { case (k, v) => k -> js.Array(v : _*) }.toSeq : _*))
}


object Animade {
    @JSExportAll
    case class Configuration (
        source : String,
        canvas : dom.Element
    )

    @JSName("Animade")
    class AnimadeJs(configuration : Configuration) extends js.Object {
        def resize(width : Int, height : Int) : Unit = js.native
        def set(uniforms : js.Dictionary[js.Array[Double]]) : Unit = js.native
        def setArray(name : String, array : js.Array[Double]) : Unit = js.native
        def draw(uniforms : js.Dictionary[js.Array[Double]]) : Unit = js.native
    }
}

