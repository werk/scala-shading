package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.js.ViewState.ListType
import dk.mzw.accelemation.js._
import dk.mzw.accelemation.js.widget.Gui._
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, Element}

class GridWidget(listType : ListType, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

    private val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Element]
    private val animade = new Animade(Animade.Configuration(buildAnimation.allList(), canvas))
    private val start = System.currentTimeMillis()

    var offsetX : Double = 0
    var offsetY : Double = 0
    var zoom : Double = 1
    var width = 0d
    var height = 0d

    override def onResize(width: Int, height: Int): Unit = {
        this.width = width
        this.height = height
        animade.resize(width, height)
    }

    override def onDraw(): Unit = {
        val now = System.currentTimeMillis()
        val t = (now - start)  / 1000.0
        animade.draw(Map(
            "u_time" -> List(t),
            "u_offset" -> List(offsetX, offsetY),
            "u_scale" -> List(zoom, zoom)
        ))
    }

    override val element: Element = div(
        "height" -> "100%",
        "width" -> "100%",
        "position" -> "relative"
    )(canvas).click(e =>
        onClick(e)
    ).toDom

    def onClick(e: MouseEvent): Unit = {
        val x = animationCoordinatesX(e.pageX)
        val y = animationCoordinatesY(e.pageY)
        println(s"Click ($x, $y)")
    }

    def animationCoordinatesX(x : Double) : Double = {
        val streched_position = (x / width) * 2 - 1
        val aspect = scala.math.max(x / height, 1.0)
        val position = streched_position * aspect
        position / zoom - offsetX
    }

    def animationCoordinatesY(reverseY : Double) : Double = {
        val y = height - reverseY
        val streched_position = (y / height) * 2 - 1
        val aspect = scala.math.max(y / width, 1.0)
        val position = streched_position * aspect
        position / zoom - offsetY
    }

}
