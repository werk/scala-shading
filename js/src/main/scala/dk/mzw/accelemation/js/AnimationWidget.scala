package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.Animation
import dk.mzw.accelemation.ToGlsl
import org.scalajs.dom
import org.scalajs.dom.Element

class AnimationWidget(animation : Animation) extends Widget {

    private val canvas = dom.document.getElementById("canvas")

    private val animade = new Animade(Animade.Configuration(ToGlsl(animation), canvas))

    private val start = System.currentTimeMillis()

    override def onResize(width: Int, height: Int): Unit = animade.resize(width, height)

    override def onDraw(): Unit = {
        val now = System.currentTimeMillis()
        animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
    }

    override val element: Element = canvas
}
