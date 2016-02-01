package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Internal
import dk.mzw.accelemation.Language.{Vec2, R, Term}
import dk.mzw.accelemation.js._
import dk.mzw.accelemation.js.widget.Gui._
import org.scalajs.dom
import org.scalajs.dom.Element

class ScrollListWidget(buildAnimation : BuildAnimation) extends Widget {

    private val animationCount = buildAnimation.animationCount
    private val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Element]
    private val animade = new Animade(Animade.Configuration(buildAnimation.allList(), canvas))
    private val start = System.currentTimeMillis()

    private val towerInner = div("width" -> "100%").toDom
    private val tower = Gui.fullSize()(towerInner).style("overflow-y" -> "scroll").toDom

    var baseOffset : Double = 0
    override def onResize(width: Int, height: Int): Unit = {
        val animationHeightPx = Math.min(width, height)
        towerInner.style.height = s"${animationCount * animationHeightPx}px"


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
            "position" -> "relative",
            "height" -> "100%"
        )(canvas, tower).toDom
    }


    var lastScrollPercent = 0d
    private def setParameters(t : Double) {
        val scrollPercent = tower.scrollTop / towerInner.offsetHeight
        if(lastScrollPercent != scrollPercent) {
            println(scrollPercent)
        }
        lastScrollPercent = scrollPercent
        val scroll = scrollPercent * 2 * animationCount
        val y = baseOffset + scroll
        animade.set(Map("u_offset" -> List(0, y)))
    }

}
