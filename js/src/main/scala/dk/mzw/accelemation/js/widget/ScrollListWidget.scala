package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language.Animation
import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.js.ViewState.{Pick0, Pick1, Pick2, ShowList, ShowSave}
import dk.mzw.accelemation.js._
import dk.mzw.accelemation.js.widget.Gui._
import dk.mzw.accelemation.samples.{HidingDevils, TimeLens}
import org.scalajs.dom
import org.scalajs.dom.Element

class ScrollListWidget(buildAnimation : BuildAnimation) extends Widget {

    private val canvas = dom.document.createElement("canvas")
    private val animade = new Animade(Animade.Configuration(ToGlsl(Scroller.scaled), canvas))
    private val start = System.currentTimeMillis()

    override def onResize(width: Int, height: Int): Unit = animade.resize(width, height)

    override def onDraw(): Unit = {
        val now = System.currentTimeMillis()
        animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
    }

    override val element: Element = {
        div()(canvas).toDom
    }

}
