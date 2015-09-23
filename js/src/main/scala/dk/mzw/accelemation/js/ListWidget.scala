package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.{R, Animation}
import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.js.BuildOrder.{Combine, Id}
import dk.mzw.accelemation.js.ViewState._
import org.scalajs.dom

class ListWidget(listType : ListType, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

    def createCanvas(name : String, source : String, newViewState : ViewState) : (String, dom.Element, Animade) = {
        val element = dom.document.createElement("div")
        val canvasElement = dom.document.createElement("canvas")
        val nameElement = dom.document.createElement("div")
        canvasElement.setAttribute("style", "box-sizing: border-box; height: 200px; width: 200px; display: inline-block; vertical-align: top;")
        nameElement.setAttribute("style", "box-sizing: border-box; padding: 20px; font-size: 20px; height: 150px; width: 200px; display: inline-block; vertical-align: top;")
        nameElement.appendChild(dom.document.createTextNode(name))
        val sourceElement = dom.document.createElement("div")
        sourceElement.setAttribute("style", "box-sizing: border-box; padding: 20px; font-size: 20px; height: 150px; width: 200px; display: inline-block; vertical-align: top;")
        sourceElement.appendChild(dom.document.createTextNode("size: " + source.length))

        element.appendChild(canvasElement)
        element.appendChild(nameElement)
        element.appendChild(sourceElement)
        element.addEventListener("click", { _ : dom.Event =>
            setViewState(newViewState)
        })
        val animade = new Animade(Animade.Configuration(source, canvasElement))
        (name, element, animade)
    }

    private val elements = listType match {
        case Pick0 =>
            buildAnimation.animations.keys.map { id =>
                val build = BuildOrder(id, Seq())
                val animation = buildAnimation(build)
                createCanvas(id.name, ToGlsl(animation), ShowAnimation(build))
            }
        case Pick1(current) =>
            buildAnimation.effects.keys.map {id =>
                def buildBuild(factor : Double) = {
                    val moreActions = Seq()
                    current.copy(actions = current.actions ++ moreActions)
                }
                val animation = buildAnimation(buildBuild(0.6))
                def effect(factor : R) : Animation = null
                createCanvas(id.name, ToGlsl(animation), ShowParameters(effect, buildBuild))
            }
        case Pick2(current, None) =>
            buildAnimation.animations.keys.map { id =>
                val animation = buildAnimation(current)
                createCanvas(id.name, ToGlsl(animation), ShowList(Pick2(current, Some(id))))
            }
        case Pick2(current, Some(animationId)) =>
            buildAnimation.combinators.keys.map { combineId =>
                val build = current.copy(actions = current.actions :+ Combine(animationId, combineId, flipped = false))
                val animation = buildAnimation(build)
                createCanvas(combineId.name, ToGlsl(animation), ShowAnimation(build))
            }
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
        val listElement = dom.document.createElement("div")
        listElement.setAttribute("style", "position: absolute; top: 0; bottom: 0; left: 0; right: 0; overflow-y: scroll")
        for((_, e, _) <- elements) listElement.appendChild(e)
        listElement
    }
}
