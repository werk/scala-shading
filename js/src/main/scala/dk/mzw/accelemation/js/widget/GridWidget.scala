package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.js.BuildOrder.{Combine, Effect}
import dk.mzw.accelemation.js.ViewState._
import dk.mzw.accelemation.js._
import dk.mzw.accelemation.js.widget.Gui._
import org.scalajs.dom
import org.scalajs.dom.Element
import dk.mzw.accelemation.js.Zoomable
import scala.scalajs.js

import scala.scalajs.js

class GridWidget(listType : ListType, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

    val (glsl, onClickAnimation) : (String, (Int, Int) => Unit) = listType match {
        case Pick0 =>
            val (pointMap, code) = buildAnimation.savedGrid()
            def clickToViewState(x : Int, y : Int) : Unit = {
                pointMap.get((x, y)).foreach{clickedBuildOrder =>
                    val nextState = ShowAnimation(clickedBuildOrder)
                    setViewState(nextState)
                }
            }
            code -> clickToViewState
        case Pick1(current) =>
            val list = buildAnimation.effects.map {effectId =>
                def effect(a : BuildOrder)(factor : R) : BuildOrder = {
                    a.copy(actions = current.actions :+ Effect(factor, effectId))
                }
                effect(current) _
            }
            val (pointMap, code) = buildAnimation.grid(list.map(_(0.6)))
            def clickToViewState(x : Int, y : Int) : Unit = {
                pointMap.get((x, y)).foreach{index =>
                    val nextState = ShowParameters(list(index))
                    setViewState(nextState)
                }
            }
            code -> clickToViewState
        case Pick2(current, None) =>
            val (pointMap, code) = buildAnimation.savedGrid()
            def clickToViewState(x : Int, y : Int) : Unit = {
                pointMap.get((x, y)).foreach{clickedBuildOrder =>
                    val id = clickedBuildOrder.animationId
                    val nextState = ShowGrid(Pick2(current, Some((id, false))))
                    setViewState(nextState)
                }
            }
            code -> clickToViewState
        case Pick2(current, Some((animationId, flipped))) =>
            val buildOrders = buildAnimation.combinators.map { combineId =>
                current.copy(actions = current.actions :+ Combine(animationId, combineId, flipped = flipped))
            }
            val (pointMap, code) = buildAnimation.grid(buildOrders)
            def clickToViewState(x : Int, y : Int) : Unit = {
                pointMap.get((x, y)).foreach{index =>
                    val nextState = ShowAnimation(buildOrders(index))
                    setViewState(nextState)
                }
            }
            code -> clickToViewState
    }

    private val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Element]
    private val panZoom = Zoomable(canvas)
    private val animade = new Animade(Animade.Configuration(glsl, canvas))
    private val start = System.currentTimeMillis()

    var offsetX : Double = 0
    var offsetY : Double = 0
    var zoom : Double = 1
    var width = 0d
    var height = 0d

    val dynamicWindow = dom.window.asInstanceOf[js.Dynamic]

    override def onResize(width: Int, height: Int): Unit = {
        this.width = width
        this.height = height
        animade.resize(width, height)
    }

    override def onDraw(): Unit = {
        val now = System.currentTimeMillis()
        val t = (now - start)  / 1000.0
        val smallest = scala.math.min(width, height)
        offsetX = panZoom.x.asInstanceOf[Double] / smallest * 2
        offsetY = -panZoom.y.asInstanceOf[Double] / smallest * 2
        zoom = panZoom.scale.asInstanceOf[Double]
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
    )(canvas).toDom



    def onClick(x : Double, y : Double): Unit = {
        val animationX = animationCoordinatesX(x)
        val animationY = animationCoordinatesY(y)
        val intX = scala.math.floor(animationX)
        val intY = scala.math.floor(animationY)
        onClickAnimation(intX.toInt, intY.toInt)
        onClickAnimation(intX.toInt, intY.toInt + 1)
        onClickAnimation(intX.toInt + 1, intY.toInt)
        onClickAnimation(intX.toInt + 1, intY.toInt + 1)
    }

    panZoom.addClickHandler(onClick _)
    panZoom.scale = 0.14

    def animationCoordinatesX(x : Double) : Double = {
        val streched_position = (x / width) * 2 - 1
        val aspect = scala.math.max(width / height, 1.0)
        val position = streched_position * aspect
        position / zoom - offsetX
    }

    def animationCoordinatesY(reverseY : Double) : Double = {
        val y = height - reverseY
        val streched_position = (y / height) * 2 - 1
        val aspect = scala.math.max(height / width, 1.0)
        val position = streched_position * aspect
        position / zoom - offsetY
    }

}
