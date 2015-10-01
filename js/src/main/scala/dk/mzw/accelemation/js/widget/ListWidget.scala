package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language.R
import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.js.BuildOrder.{Combine, Effect}
import dk.mzw.accelemation.js.ViewState._
import dk.mzw.accelemation.js.{Animade, BuildAnimation, BuildOrder, ViewState}
import org.scalajs.dom
import Gui._

class ListWidget(listType : ListType, page : Int, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

    def createCanvas(name : String, source : String, newViewState : ViewState) : (String, RichElement, Animade) = {
        val canvasDomElement = tag("canvas")(
            "height" -> "100%",
            "width" -> "100%"
        ).toDom
        val canvasElement : DomElement = canvasDomElement
        val nameElement = span()(name)
        val sourceElement = span()("size: " + source.length)
        val infoElement = div(
            "font-size" -> "20px",
            "position" -> "absolute",
            "bottom" -> "0",
            "left" -> "0",
            "width" -> "100%",
            "text-align" -> "left",
            "color" -> "white",
            "font-weight" -> "bold",
            "background-color" -> "rgba(100, 100, 100, 0.5)"
        )(nameElement, sourceElement)

        val element = div()(canvasElement, infoElement).click(() =>
            setViewState(newViewState)
        )
        val animade = new Animade(Animade.Configuration(source, canvasDomElement))
        (name, element, animade)
    }
    
    def page[E](list : Seq[E]) : Seq[E] = list.drop(page * 6)
    def previousPage = Some(page - 1).filter(_ >= 0)
    def makeNextPage(items : Int) = Some(page + 1).filter(p => (p * 6) <  items)

    private val (elements, nextPage) = listType match {
        case Pick0 =>
            val list = buildAnimation.animations
            page(list).map { id =>
                val build = BuildOrder(id, Seq())
                val animation = buildAnimation(build)
                createCanvas(id.key, ToGlsl(animation), ShowAnimation(build))
            } -> makeNextPage(list.size)
        case Pick1(current) =>
            val list = buildAnimation.effects
            page(list).map {effectId =>
                def effect(factor : R) : BuildOrder = {
                    current.copy(actions = current.actions :+ Effect(factor, effectId))
                }
                val animation = buildAnimation(effect(0.6))
                createCanvas(effectId.key, ToGlsl(animation), ShowParameters(effect))
            } -> makeNextPage(list.size)
        case Pick2(current, None) =>
            val list = buildAnimation.animations
            page(list).map { id =>
                val animation = buildAnimation(BuildOrder(id, Seq()))
                createCanvas(id.key, ToGlsl(animation), ShowList(Pick2(current, Some(id)), 0))
            } -> makeNextPage(list.size)
        case Pick2(current, Some(animationId)) =>
            val list = buildAnimation.combinators
            page(list).map { combineId =>
                val build = current.copy(actions = current.actions :+ Combine(animationId, combineId, flipped = false))
                val animation = buildAnimation(build)
                createCanvas(combineId.key, ToGlsl(animation), ShowAnimation(build))
            } -> makeNextPage(list.size)
    }


    private val start = System.currentTimeMillis()

    override def onResize(width : Int, height : Int) : Unit = {
        for((_, _, animade) <- elements) {
            animade.resize(200, 200) // TODO
        }
    }

    override def onDraw() : Unit = {
        val now = System.currentTimeMillis()
        for((_, _, animade) <- elements) {
            animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
        }
    }

    override val element : dom.Element = {
        val td = tag("td")(
            "padding" -> "0",
            "width" -> "33.3333333333%",
            "position" -> "relative"
        )

        val tr = tag("tr")()

        val es = elements.map(_._2)

        val table = tag("table")(
            "height" -> "100%",
            "width" -> "100%"
        )(
            tr(td.flatAppend(es.lift(0)), td.flatAppend(es.lift(1)), td.flatAppend(es.lift(2))),
            tr(td.flatAppend(es.lift(3)), td.flatAppend(es.lift(4)), td.flatAppend(es.lift(5)))
        )
        table.toDom
    }
}
