package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.Language.{R, Term}
import dk.mzw.accelemation.js.ViewState._
import dk.mzw.accelemation.js.widget.Gui._
import dk.mzw.accelemation.js._
import dk.mzw.accelemation.{Internal, ToGlsl}
import org.scalajs.dom


class SaveWidget(build : BuildOrder, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

    private val (containerElement, animade) = {
        val canvasDomElement = tag("canvas")(
            "height" -> "100%",
            "width" -> "100%"
        ).toDom

        var nameText = ""

        val animation = buildAnimation(build)
        val source = ToGlsl(animation)
        val animade : Animade = new Animade(Animade.Configuration(source, canvasDomElement))

        val nameElement = {
            tag("input", "placeholder" -> "Name your animation...", "autofocus" -> "autofocus")()().style(
                "position" -> "absolute",
                "left" -> "50%",
                "bottom" -> "1%",
                "width" -> "200px",
                "height" -> "40px",
                "padding-left" -> "10px",
                "padding-right" -> "10px",
                "transform" -> "translate(-50%, -100%)",
                "box-shadow" -> "0 0 0 15px rgba(100, 100, 100, 0.5)",
                "border" -> "none",
                "outline" -> "none",
                "border-radius" -> "20px"
            ).change { event =>
                nameText = event.target.asInstanceOf[dom.html.Input].value.trim
            }
        }

        val cancelButton = {
            def cancel() : Unit = {
                setViewState(ShowAnimation(build))
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
                if(nameText.isEmpty) dom.window.alert("Please name your animation.")
                else LocalStore.store.put(nameText, build, onSuccess = { _ => setViewState(ShowList(Pick0, 0, None))}, onError = println)
            }
            val e = tag("i")()().addClasses("fa", "fa-5x", "fa-check-circle")
            roundButton(e, "rgba(100, 100, 100, 0.5)", ok()).style(
                "position" -> "absolute",
                "left" -> "100%",
                "bottom" -> "0",
                "transform" -> "translate(-100%, 0)"
            )
        }

        val element = fullSize()(canvasDomElement, nameElement, cancelButton, okButton)
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
