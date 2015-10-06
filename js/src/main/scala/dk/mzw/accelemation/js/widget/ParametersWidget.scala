package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language.{R, Term}
import dk.mzw.accelemation.js.ViewState._
import dk.mzw.accelemation.js.{Animade, BuildAnimation, BuildOrder, ViewState}
import dk.mzw.accelemation.{Internal, ToGlsl}
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLInputElement
import Gui._


class ParametersWidget(effect : R => BuildOrder, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

    private var parameter = 0.6

    private val (containerElement, animade) = {
        val canvasDomElement = tag("canvas")(
            "height" -> "100%",
            "width" -> "100%"
        ).toDom

        val build = effect(Term(Internal.BuiltIn("u_parameter")))
        val animation = buildAnimation(build)
        val source = "uniform float u_parameter;\n\n" + ToGlsl(animation)
        val animade : Animade = new Animade(Animade.Configuration(source, canvasDomElement))
        animade.set(Map("u_parameter" -> List(parameter)))

        val sliderDomElement = tag("input")().toDom
        sliderDomElement.setAttribute("type", "range")
        sliderDomElement.setAttribute("min", "0")
        sliderDomElement.setAttribute("value", (parameter * 100).toInt.toString)
        sliderDomElement.setAttribute("max", "100")
        sliderDomElement.setAttribute("step", "1")
        sliderDomElement.addEventListener("change", { _ : dom.Event =>
            parameter = sliderDomElement.asInstanceOf[HTMLInputElement].value.toDouble / 100.0
            animade.set(Map("u_parameter" -> List(parameter)))
        })
        sliderDomElement.addEventListener("mousemove", { _ : dom.Event =>
            parameter = sliderDomElement.asInstanceOf[HTMLInputElement].value.toDouble / 100.0
            animade.set(Map("u_parameter" -> List(parameter)))
        })

        val buttonElement = tag("button")()("OK").click(() => setViewState(ShowAnimation(effect(parameter))))
        val parametersElement = div(
            "position" -> "absolute",
            "height" -> "300px",
            "width" -> "300px",
            "top" -> "50%",
            "left" -> "50%",
            "transform" -> "translate(-50%, -50%)",
            "padding" -> "50px",
            "font-size" -> "20px",
            "color" -> "white",
            "border-radius" -> "100%",
            "background-color" -> "rgba(100, 100, 100, 0.5)",
            "text-align" -> "center"
        )(sliderDomElement, div()(buttonElement))

        val element = fullSize()(canvasDomElement, parametersElement)
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
