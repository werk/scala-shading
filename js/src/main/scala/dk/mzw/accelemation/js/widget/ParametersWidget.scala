package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language.{R, Term}
import dk.mzw.accelemation.js.ViewState._
import dk.mzw.accelemation.js.{Animade, BuildAnimation, BuildOrder, ViewState}
import dk.mzw.accelemation.{Internal, ToGlsl}
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLInputElement
import Gui._


class ParametersWidget(effect : R => BuildOrder, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

    private val (containerElement, animade) = {
        val canvasDomElement = tag("canvas")(
            "height" -> "100%",
            "width" -> "100%"
        ).toDom

        var angle = 6.0

        val build = effect(Term(Internal.BuiltIn("u_parameter")))
        val animation = buildAnimation(build)
        val source = "uniform float u_parameter;\n\n" + ToGlsl(animation)
        val animade : Animade = new Animade(Animade.Configuration(source, canvasDomElement))
        animade.set(Map("u_parameter" -> List(angle / 10)))

        def offset(radians : Double, magnitude : Double) : (Double, Double) = {
            val x = magnitude * Math.cos(radians)
            val y = magnitude * Math.sin(radians) - 38.5
            (x, -y)
        }

        def onMouseMove(event : dom.MouseEvent) : Unit = {
            if(event.buttons != 1) return
            def mod(p : Double, q : Double) = ((p % q) + q) % q
            val target = event.currentTarget.asInstanceOf[dom.html.Element]
            val clickX = event.pageX - target.offsetLeft
            val clickY = event.pageY - target.offsetTop
            val shownAngle = mod(angle, Math.PI * 2)
            val clickAngle = Math.PI / 2 + Math.atan2(clickY, clickX)
            val angleDifference = clickAngle - shownAngle
            val angleDelta = mod(angleDifference + Math.PI, Math.PI * 2) - Math.PI
            angle += angleDelta
            val (x, y) = offset(Math.PI / 2 - angle, 25)
            val e = dom.document.getElementsByClassName("dial")(0).asInstanceOf[dom.html.Element]
            e.style.marginLeft = Math.round(x) + "px"
            e.style.marginTop = Math.round(y - 4) + "px"
            animade.set(Map("u_parameter" -> List(angle / 10)))
        }

        val parametersElement = {
            val dotCount = 12
            val dots = for(a <- 0 until dotCount) yield {
                val (x, y) = offset(a * 2 * Math.PI / dotCount, 42)
                val y2 = y - (if(a % (dotCount / 4) == 0) 2 else 0)
                val s = if(a % (dotCount / 4) == 0) 6 else 4
                tag("i")("margin-left" -> (Math.round(x) + "px"), "margin-top" -> (Math.round(y2) + "px"), "font-size" -> (s + "px"))().addClasses("fa", "fa-stack-1x", "fa-circle")
            }
            val (x, y) = offset(Math.PI / 2 - angle, 25)
            val e = tag("span")("user-select" -> "none", "-webkit-user-select" -> "none")(
                tag("i")()().addClasses("fa", "fa-5x", "fa-circle") +:
                tag("i")("margin-left" -> (Math.round(x) + "px"), "margin-top" -> (Math.round(y - 4) + "px"), "color" -> "#707070", "font-size" -> "10px")().addClasses("fa", "fa-stack-1x", "fa-circle", "dial") +:
                dots
            : _*)
            val b = roundButton(e, "rgba(100, 100, 100, 0.5)", {}).mouseMove(onMouseMove).mouseDown(onMouseMove)
            b.style(
                "cursor" -> "grab",
                "position" -> "absolute",
                "left" -> "50%",
                "bottom" -> "0",
                "transform" -> "translate(-50%, -50%)"
            )
        }

        val cancelButton = {
            def cancel() : Unit = {
                val animation = effect(0)
                setViewState(ShowList(Pick1(animation.copy(actions = animation.actions.init)), 0))
            }
            val e = tag("i")()().addClasses("fa", "fa-5x", "fa-times-circle")
            roundButton(e, "rgba(100, 100, 100, 0.5)", cancel()).style(
                "position" -> "absolute",
                "left" -> "0",
                "bottom" -> "0",
                "transform" -> "translate(0, 0)"
            )
        }

        val okButton = {
            def ok() : Unit = {
                val animation = effect(angle / 10)
                setViewState(ShowAnimation(animation))
            }
            val e = tag("i")()().addClasses("fa", "fa-5x", "fa-check-circle")
            roundButton(e, "rgba(100, 100, 100, 0.5)", ok()).style(
                "position" -> "absolute",
                "left" -> "100%",
                "bottom" -> "0",
                "transform" -> "translate(-100%, 0)"
            )
        }

        val element = fullSize()(canvasDomElement, parametersElement, cancelButton, okButton)
        (element, animade)
    }

    override val element = containerElement.toDom

    private val start = System.currentTimeMillis()

    override def onResize(width : Int, height : Int) : Unit = {
        animade.resize(width, height)
    }

    override def onDraw() : Unit = {
        val now = System.currentTimeMillis()
        animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
    }
}
