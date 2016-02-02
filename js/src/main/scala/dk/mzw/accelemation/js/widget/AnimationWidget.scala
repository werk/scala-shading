package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.js.ViewState._
import dk.mzw.accelemation.js._
import org.scalajs.dom
import org.scalajs.dom.Element
import Gui._

class AnimationWidget(build : BuildOrder, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

    private val canvas = dom.document.createElement("canvas")
    private val animade = new Animade(Animade.Configuration(buildAnimation.toGlsl(build), canvas))
    private val start = System.currentTimeMillis()

    override def onResize(width: Int, height: Int): Unit = animade.resize(width, height)

    override def onDraw(): Unit = {
        val now = System.currentTimeMillis()
        animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
    }

    override val element: Element = {
        val menu = div(
            "position" -> "absolute",
            "bottom" -> "20px",
            "left" -> "0",
            "height" -> "100px",
            "width" -> "100%",
            "text-align" -> "center"
        )(
            roundButton("Discard", "rgba(200, 100, 100, 0.5)", {setViewState(ShowGrid(Pick0))}).style("margin" -> "5px"),
            roundButton("Effect", "rgba(200, 100, 200, 0.5)", {setViewState(ShowGrid(Pick1(build)))}).style("margin" -> "5px"),
            roundButton("Combine", "rgba(100, 100, 200, 0.5)", {setViewState(ShowGrid(Pick2(build, None)))}).style("margin" -> "5px"),
            roundButton("Save", "rgba(100, 200, 100, 0.5)", {setViewState(ShowSave(build))}).style("margin" -> "5px")
        )

        div()(canvas, menu).toDom
    }

}
