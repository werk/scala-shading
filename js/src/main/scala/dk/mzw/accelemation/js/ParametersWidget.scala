package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.{Animation, R, Term}
import dk.mzw.accelemation.js.ViewState._
import dk.mzw.accelemation.{Internal, ToGlsl}
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLInputElement

class ParametersWidget(effect : R => Animation, setViewState : ViewState => Unit) extends Widget {

  private val parameter = 0.5

  private val (containerElement, animade) = {
    val element = dom.document.createElement("div")
    val canvasElement = dom.document.createElement("canvas")
    val parametersElement = dom.document.createElement("div")
    val sliderElement = dom.document.createElement("input")
    val buttonElement = dom.document.createElement("button")
    canvasElement.setAttribute("style", "box-sizing: border-box; height: 50%; width: 100%;")
    parametersElement.setAttribute("style", "box-sizing: border-box; padding: 20px; font-size: 20px; height: 50%; width: 100%;")
    parametersElement.appendChild(sliderElement)
    parametersElement.appendChild(buttonElement)
    element.appendChild(canvasElement)
    element.appendChild(parametersElement)
    buttonElement.addEventListener("click", { _ : dom.Event =>
      setViewState(ShowAnimation(effect(parameter)))
    })
    buttonElement.appendChild(dom.document.createTextNode("OK"))
    val source = "uniform float u_parameter;\n\n" + ToGlsl(effect(Term(Internal.BuiltIn("u_parameter"))))
    val a : Animade = new Animade(Animade.Configuration(source, canvasElement))
    a.set(Map("u_parameter" -> List(parameter)))
    sliderElement.setAttribute("type", "range")
    sliderElement.setAttribute("min", "0")
    sliderElement.setAttribute("max", "100")
    sliderElement.setAttribute("step", "1")
    sliderElement.addEventListener("change", { _ : dom.Event =>
      a.set(Map("u_parameter" -> List(sliderElement.asInstanceOf[HTMLInputElement].value.toDouble / 100.0)))
    })
    sliderElement.addEventListener("mousemove", { _ : dom.Event =>
        a.set(Map("u_parameter" -> List(sliderElement.asInstanceOf[HTMLInputElement].value.toDouble / 100.0)))
    })
    (element, a)
  }

  override val element = containerElement

  private val start = System.currentTimeMillis()

  override def onResize(width : Int, height : Int) : Unit = {
    animade.resize(width, height / 2)
  }

  override def onDraw() : Unit = {
    val now = System.currentTimeMillis()
    animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
  }
}
