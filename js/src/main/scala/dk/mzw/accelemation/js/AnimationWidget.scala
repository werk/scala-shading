package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.Animation
import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.js.ViewState.{Pick2, Pick1, ShowList, Pick0}
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.Element

class AnimationWidget(animation : Animation, setViewState : ViewState => Unit) extends Widget {

    private val canvas = dom.document.createElement("canvas")
    private val animade = new Animade(Animade.Configuration(ToGlsl(animation), canvas))
    private val start = System.currentTimeMillis()

    override def onResize(width: Int, height: Int): Unit = animade.resize(width, height)

    override def onDraw(): Unit = {
        val now = System.currentTimeMillis()
        animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
    }

    override val element: Element = {
        def button(name : String, color : String, click : Unit => ())  = Div(
            "display" -> "inline-block",
            "height" -> "100%",
            "width" -> "33%",
            "background-color" -> color
        ).click(click)(name)

        val menu = Div("position" -> "absolute", "bottom" -> "0", "right" -> "0", "height" -> "20%", "width" -> "50%")(
            button("New", "rgba(1, 0, 0, 0.5)", {_ => setViewState(ShowList(Pick0))}),
            button("Effect", "rgba(0, 1, 0, 0.5)", {_ => setViewState(ShowList(Pick1(animation)))}),
            button("Combine", "rgba(0, 0, 1, 0.5)", {_ => setViewState(ShowList(Pick2(animation, None)))})
        )

        canvas.addEventListener("click", { _ : dom.Event =>
            setViewState(ShowList(Pick0))
        })

        Div()(canvas, menu)
    }

    case class Div(styles : (String, String)*) {
        private var onClicks = List[Unit => ()]()

        def click(handler : Unit => ()) : Div = {
            onClicks ::= handler
            this
        }

        def apply(children : dom.Element*) : dom.Element = {
            val e = create()
            children.foreach(e.appendChild)
            e
        }

        def apply(text : String) : dom.Element = {
            val e = create()
            e.appendChild(document.createTextNode(text))
            e
        }

        private def create() = {
            val e = document.createElement("div")
            e.setAttribute("style", styles.map{case (k, v) => s"$k: $v; "}.mkString)
            if(onClicks.nonEmpty)
            e.addEventListener("click", { _ : dom.Event =>
                onClicks.foreach(_())
            })
            e
        }

    }
}
