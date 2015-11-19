package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language.R
import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.js.BuildOrder.{Combine, Effect}
import dk.mzw.accelemation.js.ViewState._
import dk.mzw.accelemation.js.widget.Widgets.Deck
import dk.mzw.accelemation.js.{Animade, BuildAnimation, BuildOrder, ViewState}
import org.scalajs.dom
import Gui._

class ListWidget(listType : ListType, page : Int, filter : Option[String], setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

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
    
    def page[E](list : Seq[E]) : Seq[E] = list.drop(page * 6).take(6)
    def previousPage = Some(page - 1).filter(_ >= 0)
    def makeNextPage(items : Int) = Some(page + 1).filter(p => (p * 6) <  items)

    private val (elements, nextPage) = listType match {
        case Pick0 =>
            val list = buildAnimation.animations.filter{ id =>
                filter.map(f => buildAnimation.nameMap(id).toLowerCase.contains(f.toLowerCase)).getOrElse(true)
            }
            page(list).map { id =>
                val build = BuildOrder(None, id, Seq())
                val animation = buildAnimation(build)
                createCanvas(buildAnimation.nameMap(id), ToGlsl(animation), ShowAnimation(build))
            } -> makeNextPage(list.size)
        case Pick1(current) =>
            val list = buildAnimation.effects
            page(list).map {effectId =>
                def effect(factor : R) : BuildOrder = {
                    current.copy(actions = current.actions :+ Effect(factor, effectId))
                }
                val animation = buildAnimation(effect(0.6))
                createCanvas(buildAnimation.nameMap(effectId), ToGlsl(animation), ShowParameters(effect))
            } -> makeNextPage(list.size)
        case Pick2(current, None) =>
            val list = buildAnimation.animations
            page(list).map { id =>
                val animation = buildAnimation(BuildOrder(None, id, Seq()))
                createCanvas(buildAnimation.nameMap(id), ToGlsl(animation), ShowList(Pick2(current, Some((id, false))), 0, None))
            } -> makeNextPage(list.size)
        case Pick2(current, Some((animationId, flipped))) =>
            val list = buildAnimation.combinators
            page(list).map { combineId =>
                val build = current.copy(actions = current.actions :+ Combine(animationId, combineId, flipped = flipped))
                val animation = buildAnimation(build)
                createCanvas(buildAnimation.nameMap(combineId), ToGlsl(animation), ShowAnimation(build))
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
            val b = roundButton(i, "rgba(100, 100, 100, 0.5)", setViewState(ShowList(listType, p, filter)))
            b.style(
                "position" -> "absolute",
                "left" -> "5px",
                "top" -> "50%",
                "transform" -> "translate(0, -50%)"
            )
        }

        val next = nextPage.map{n =>
            val i = tag("i")()().addClasses("fa", "fa-5x", "fa-chevron-circle-right")
            val b = roundButton(i, "rgba(100, 100, 100, 0.5)", setViewState(ShowList(listType, n, filter)))
            b.style(
                "position" -> "absolute",
                "right" -> "5px",
                "top" -> "50%",
                "transform" -> "translate(0, -50%)"
            )
        }

        val flip = Some(listType).collect{ case Pick2(current, Some((animationId, flipped))) =>
            val i = tag("i")()().addClasses("fa", "fa-5x", "fa-arrows-h")
            val flipStage = ShowList(Pick2(current, Some((animationId, !flipped))), page, filter)
            val b = roundButton(i, "rgba(100, 100, 100, 0.5)", setViewState(flipStage))
            b.style(
                "position" -> "absolute",
                "top" -> "5px",
                "left" -> "50%",
                "transform" -> "translate(-50%, 0)"
            )
        }

        val find = Some(listType).collect{ case Pick0 =>
            var deck : Deck[String] = null
            val i = tag("i")()().addClasses("fa", "fa-3x", "fa-search")
            val button = roundButton(i, "rgba(100, 100, 100, 0.5)", {deck.show("input")}).style(
                "position" -> "absolute",
                "top" -> "5px",
                "left" -> "50%",
                "transform" -> "translate(-50%, 0)"
            ).toDom

            //searchText = event.target.asInstanceOf[dom.html.Input].value.trim
            val searchElement = tag("input", "placeholder" -> "Find animation...", "autofocus" -> "autofocus", "value" -> filter.getOrElse(""))()().style(
                "position" -> "absolute",
                "top" -> "0",
                "left" -> "0",
                "bottom" -> "0",
                "width" -> "100%",
                "border" -> "none",
                "outline" -> "none",
                "padding-left" -> "10px",
                "padding-right" -> "50px",
                "border-radius" -> "20px",
                "box-sizing" -> "border-box"
            ).toDom

            val searchButton = {
                val content = tag("i")()().addClasses("fa", "fa-1x", "fa-search")
                roundButton(content, "rgba(100, 100, 100, 1)", {
                    val filterText = searchElement.asInstanceOf[dom.html.Input].value.trim
                    setViewState(ShowList(listType, page, Some(filterText).filter(_.trim.nonEmpty)))
                }).style(
                    "position" -> "absolute",
                    "top" -> "50%",
                    "right" -> "0",
                    "transform" -> "translate(0, -50%)",
                    "height" -> "40px",
                    "width" -> "40px"
                )
            }

            val search = {
                div(
                    "position" -> "absolute",
                    "top" -> "20px",
                    "left" -> "50%",
                    "transform" -> "translate(-50%, 0)",
                    "width" -> "200px",
                    "height" -> "40px",
                    "box-shadow" -> "0 0 0 15px rgba(100, 100, 100, 0.5)",
                    "border" -> "none",
                    "outline" -> "none",
                    "border-radius" -> "20px"
                )(searchElement, searchButton)
            }.toDom

            deck = new Deck(Map("button" -> button, "input" -> search), if(filter.isDefined) "input" else "button")
            deck

        }

        val cancel = {
            val i = tag("i")()().addClasses("fa", "fa-5x", "fa-times-circle")
            val undoStage = listType match {
                case Pick0 => None
                case Pick1(current) => Some(ShowAnimation(current))
                case Pick2(current, None) => Some(ShowAnimation(current))
                case Pick2(current, Some(p)) => Some(ShowList(Pick2(current, None), page, filter))
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

        fullSize().flatAppend(Some(boxes), previous, next, flip, find, cancel).toDom
    }
}
