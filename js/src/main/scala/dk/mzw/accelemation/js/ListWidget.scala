package dk.mzw.accelemation.js

import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.js.ViewState.{Pick2, Pick1, Pick0, ListType}
import org.scalajs.dom

class ListWidget(listType : ListType) extends Widget {

    def createCanvas(name : String, source : String) : (String, dom.Element, Animade) = {
        val element = dom.document.createElement("div")
        val canvasElement = dom.document.createElement("canvas")
        val nameElement = dom.document.createElement("div")
        canvasElement.setAttribute("style", "box-sizing: border-box; height: 150px; width: 150px; display: inline-block; vertical-align: top;")
        nameElement.setAttribute("style", "box-sizing: border-box; padding: 20px; font-size: 20px; height: 150px; width: 200px; display: inline-block; vertical-align: top;")
        nameElement.appendChild(dom.document.createTextNode(name))
        element.appendChild(canvasElement)
        element.appendChild(nameElement)
        val animade = new Animade(Animade.Configuration(source, canvasElement))
        (name, element, animade)
    }

    private val elements = listType match {
        case Pick0 =>
            GlobalAnimations.animations.map {
                case (name, animation) =>
                    createCanvas(name, ToGlsl(animation))
            }
        case Pick1(current) =>
            GlobalAnimations.effects.map {
                case (name, effect) =>
                    createCanvas(name, ToGlsl(effect(current)))
            }
        case Pick2(current, None) =>
            GlobalAnimations.animations.map {
                case (name, animation) =>
                    createCanvas(name, ToGlsl(animation))
            }
        case Pick2(current, Some(argument)) =>
            GlobalAnimations.combinators.map {
                case (name, combinator) =>
                    createCanvas(name, ToGlsl(combinator(current)(argument)))
            }
    }


    private val start = System.currentTimeMillis()

    override def onResize(width : Int, height : Int) : Unit = {
        for((_, _, animade) <- elements) {
            animade.resize(width, height)
        }
    }

    override def onDraw() : Unit = {
        val now = System.currentTimeMillis()
        for((_, _, animade) <- elements) {
            animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
        }
    }

    override val element : dom.Element = {
        val listElement = dom.document.createElement("div")
        for((_, e, _) <- elements) listElement.appendChild(e)
        listElement
    }
}
