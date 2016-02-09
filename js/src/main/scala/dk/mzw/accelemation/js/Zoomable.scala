package dk.mzw.accelemation.js

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSName, JSExportAll}

object Zoomable {
    def apply(element : dom.Element) = {
        new ZoomableJs(element).panZoom
    }

    @js.native
    trait PanZoom extends js.Object {
        var x: Double
        var y: Double
        var scale: Double
        def addClickHandler(handler : js.Function2[Double, Double, Unit])
    }

    @JSName("Zoomable")
    class ZoomableJs(element : dom.Element) extends js.Object {
        val panZoom : PanZoom = js.native
    }
}



