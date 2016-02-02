package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.js._
import org.scalajs.dom
import org.scalajs.dom.Element

class ScrollListWidget(buildAnimation : BuildAnimation) extends Widget {

    private val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Element]
    private val animade = new Animade(Animade.Configuration(buildAnimation.allList(), canvas))
    private val start = System.currentTimeMillis()

    var centerX : Double = 0
    var centerY : Double = 0
    var zoom : Double = 0.1

    override def onResize(width: Int, height: Int): Unit = {
        animade.resize(width, height)
    }

    override def onDraw(): Unit = {
        val now = System.currentTimeMillis()
        val t = (now - start)  / 1000.0
        animade.draw(Map(
            "u_time" -> List(t),
            "u_offset" -> List(centerX, centerY),
            "u_scale" -> List(zoom, zoom)
        ))
    }

    override val element: Element = canvas


}
