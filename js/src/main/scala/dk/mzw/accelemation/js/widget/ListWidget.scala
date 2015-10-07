package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language.R
import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.js.BuildOrder.{Combine, Effect}
import dk.mzw.accelemation.js.ViewState._
import dk.mzw.accelemation.js.{Animade, BuildAnimation, BuildOrder, ViewState}
import org.scalajs.dom
import Gui._

class ListWidget(listType : ListType, page : Int, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

    def createCanvas(name : String, source : String, newViewState : ViewState) : CanvasBox = {
        val canvasDomElement = tag("canvas")(
            "height" -> "100%",
            "width" -> "100%"
        ).toDom
        val canvasElement : DomElement = canvasDomElement
        val nameElement = div(
            "padding" -> "6px",
            "text-align" -> "center"
        )(name)
        val infoElement = div(
            "font-size" -> "14px",
            "position" -> "absolute",
            "bottom" -> "0",
            "left" -> "0",
            "width" -> "100%",
            "color" -> "white",
            "font-weight" -> "bold",
            "background-color" -> "rgba(100, 100, 100, 0.7)"
        )(nameElement)

        val element = div(
            "height" -> "100%",
            "width" -> "100%",
            "position" -> "relative"
        )(canvasElement, infoElement).click(_ =>
            setViewState(newViewState)
        )
        val animade = new Animade(Animade.Configuration(source, canvasDomElement))
        CanvasBox(name, element, canvasDomElement, animade)
    }

    case class CanvasBox(
        name : String,
        element : RichElement,
        canvasDomElement : dom.Element,
        animade : Animade
    )
    
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
                createCanvas(id.key, ToGlsl(animation), ShowList(Pick2(current, Some((id, false))), 0))
            } -> makeNextPage(list.size)
        case Pick2(current, Some((animationId, flipped))) =>
            val list = buildAnimation.combinators
            page(list).map { combineId =>
                val build = current.copy(actions = current.actions :+ Combine(animationId, combineId, flipped = flipped))
                val animation = buildAnimation(build)
                createCanvas(combineId.key, ToGlsl(animation), ShowAnimation(build))
            } -> makeNextPage(list.size)
    }


    private val start = System.currentTimeMillis()

    override def onResize(width : Int, height : Int) : Unit = {
        for(box <- elements) {
            if(box.canvasDomElement.clientWidth == 0 && box.canvasDomElement.clientHeight == 0) return // TODO
            box.animade.resize(box.canvasDomElement.clientWidth, box.canvasDomElement.clientHeight)
            println(s"box.animade.resize(${box.canvasDomElement.clientWidth}, ${box.canvasDomElement.clientHeight})")
        }
    }

    override def onDraw() : Unit = {
        val now = System.currentTimeMillis()
        for(box <- elements) {
            box.animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
        }
    }

    override val element : dom.Element = {
        val boxes = {
            def box(e : Option[RichElement]) = {
                val inner = fullSize("5px").flatAppend(e)
                inlineBlock(
                    "width" -> "33.333%",
                    "height" -> "50%",
                    "position" -> "relative"
                )(inner)
            }

            val es = elements.map(_.element)

            fullSize().style("background-color" -> "#222")(
                fullSize("5px")(
                    box(es.lift(0)), box(es.lift(1)), box(es.lift(2)),
                    box(es.lift(3)), box(es.lift(4)), box(es.lift(5))
                )
            )
        }

        val previous = previousPage.map{ p =>
            val i = tag("i")()().addClasses("fa", "fa-5x", "fa-chevron-circle-left")
            val b = roundButton(i, "rgba(100, 100, 100, 0.5)", setViewState(ShowList(listType, p)))
            b.style(
                "position" -> "absolute",
                "left" -> "0",
                "top" -> "50%",
                "transform" -> "translate(0, -50%)"
            )
        }

        val next = nextPage.map{n =>
            val i = tag("i")()().addClasses("fa", "fa-5x", "fa-chevron-circle-right")
            val b = roundButton(i, "rgba(100, 100, 100, 0.5)", setViewState(ShowList(listType, n)))
            b.style(
                "position" -> "absolute",
                "right" -> "0",
                "top" -> "50%",
                "transform" -> "translate(0, -50%)"
            )
        }

        val flip = Some(listType).collect{ case Pick2(current, Some((animationId, flipped))) =>
            val i = tag("i")()().addClasses("fa", "fa-5x", "fa-arrows-h")
            val flipStage = ShowList(Pick2(current, Some((animationId, !flipped))), page)
            val b = roundButton(i, "rgba(100, 100, 100, 0.5)", setViewState(flipStage))
            b.style(
                "position" -> "absolute",
                "top" -> "0",
                "left" -> "50%",
                "transform" -> "translate(-50%, 0)"
            )
        }

        val cancel = {
            val i = tag("i")()().addClasses("fa", "fa-5x", "fa-times-circle")
            val undoStage = listType match {
                case Pick0 => None
                case Pick1(current) => Some(ShowAnimation(current))
                case Pick2(current, None) => Some(ShowAnimation(current))
                case Pick2(current, Some(p)) => Some(ShowList(Pick2(current, None), page))
            }
            undoStage.map{s =>
                val b = roundButton(i, "rgba(100, 100, 100, 0.5)", setViewState(s))
                b.style(
                    "position" -> "absolute",
                    "bottom" -> "0",
                    "left" -> "50%",
                    "transform" -> "translate(-50%, 0)"
                )
            }
        }

        fullSize().flatAppend(Some(boxes), previous, next, flip, cancel).toDom
    }
}
