package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Internal
import dk.mzw.accelemation.Language.{Vec2, R, Term}
import dk.mzw.accelemation.js._
import dk.mzw.accelemation.js.widget.Gui._
import org.scalajs.dom
import org.scalajs.dom.Element

class ScrollListWidget(buildAnimation : BuildAnimation) extends Widget {

    private val offset : Vec2 = Term(Internal.BuiltIn("u_offset"))

    private val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Element]
    private val animade = new Animade(Animade.Configuration(buildAnimation.allList(), canvas))
    private val start = System.currentTimeMillis()

    private val tower = div("height" -> "400%")().toDom

    var baseOffset : Double = 0
    override def onResize(width: Int, height: Int): Unit = {
        tower.style.height = (height * 2) + "px"
        baseOffset = height.toDouble / width - 1
        animade.resize(width, height)
    }

    override def onDraw(): Unit = {
        val now = System.currentTimeMillis()
        val t = (now - start)  / 1000.0
        setParameters(t)
        animade.draw(Map("u_time" -> List[Double](t)))
    }

    override val element: Element = {
        canvas.style.position = "absolute"
        canvas.style.top = "0"
        canvas.style.right = "0"
        canvas.style.bottom = "0"
        canvas.style.left = "0"
        div(
            "overflow-y" -> "scroll",
            "position" -> "relative",
            "height" -> "100%"
        )(canvas, tower).toDom
    }

    private def setParameters(t : Double) {
        val y = baseOffset
        animade.set(Map("u_offset" -> List(0, y)))
    }

}
