package dk.mzw.accelemation.js

import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.js.BuildOrder.Id
import dk.mzw.accelemation.js.ViewState.{Pick2, Pick1, ShowList, Pick0}
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.Element

class AnimationWidget(build : BuildOrder, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) extends Widget {

    private val animation = buildAnimation(build)
    println(BuildOrder.show(BuildOrder.readBuildOrder(BuildOrder.show(build))))

    private val canvas = dom.document.createElement("canvas")
    private val animade = new Animade(Animade.Configuration(ToGlsl(animation), canvas))
    private val start = System.currentTimeMillis()

    override def onResize(width: Int, height: Int): Unit = animade.resize(width, height)

    override def onDraw(): Unit = {
        val now = System.currentTimeMillis()
        animade.draw(Map("u_time" -> List[Double]((now - start) / 1000.0)))
    }

    override val element: Element = {
        def button(name : String, color : String, click : () => Unit) = {
            val td = DomElement("td")(
                "text-align" -> "center",
                "vertical-align" -> "middle",
                "height" -> "100%",
                "width" -> "100%",
                "color" -> "white",
                "font-weight" -> "bold"
            )(name)
            val tr = DomElement("tr")()(td)
            val table = DomElement("table")(
                "height" -> "100px",
                "width" -> "100px",
                "margin-left" -> "5px",
                "margin-right" -> "5px",
                "border-radius" -> "100%",
                "background-color" -> color,
                "cursor" -> "pointer"
            ).click(click)(tr)
            div("display" -> "inline-block")(table)
        }

        val menu = div("position" -> "absolute", "bottom" -> "20px", "left" -> "0", "height" -> "100px", "width" -> "100%", "text-align" -> "center")(
            button("Discard", "rgba(200, 100, 100, 0.5)", {() => setViewState(ShowList(Pick0))}),
            button("Effect", "rgba(200, 100, 200, 0.5)", {() => setViewState(ShowList(Pick1(build)))}),
            button("Combine", "rgba(100, 100, 200, 0.5)", {() => setViewState(ShowList(Pick2(build, None)))}),
            button("Save", "rgba(100, 200, 100, 0.5)", {() =>
                def onSave(id : Id) : Unit = {
                    println("Saved: " + BuildOrder.show(id))
                    setViewState(ShowList(Pick0))
                }
                LocalStore.store.put("TODO", build, onSuccess = onSave, onError = println)
            })
        )

        div()(canvas, menu)
    }

    def div(styles : (String, String)*) = DomElement("div")(styles : _*)

    case class DomElement(tag : String)(styles : (String, String)*) {
        private var onClicks = List[() => Unit]()

        def click(handler : () => Unit) : DomElement = {
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
            val e = document.createElement(tag)
            e.setAttribute("style", styles.map{case (k, v) => s"$k: $v; "}.mkString)
            if(onClicks.nonEmpty)
            e.addEventListener("click", { _ : dom.Event =>
                onClicks.foreach(_())
            })
            e
        }
    }
}
