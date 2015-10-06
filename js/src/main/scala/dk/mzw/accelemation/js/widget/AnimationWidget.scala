package dk.mzw.accelemation.js.widget

import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.js.BuildOrder.Id
import dk.mzw.accelemation.js.ViewState.{Pick0, Pick1, Pick2, ShowList}
import dk.mzw.accelemation.js._
import org.scalajs.dom
import org.scalajs.dom.Element
import Gui._

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
        val menu = div(
            "position" -> "absolute",
            "bottom" -> "20px",
            "left" -> "0",
            "height" -> "100px",
            "width" -> "100%",
            "text-align" -> "center"
        )(
            roundButton("Discard", "rgba(200, 100, 100, 0.5)", {setViewState(ShowList(Pick0, 0))}),
            roundButton("Effect", "rgba(200, 100, 200, 0.5)", {setViewState(ShowList(Pick1(build), 0))}),
            roundButton("Combine", "rgba(100, 100, 200, 0.5)", {setViewState(ShowList(Pick2(build, None), 0))}),
            roundButton("Save", "rgba(100, 200, 100, 0.5)", {
                def onSave(id : Id) : Unit = {
                    println("Saved: " + BuildOrder.show(id))
                    setViewState(ShowList(Pick0, 0))
                }
                LocalStore.store.put("TODO", build, onSuccess = onSave, onError = println)
            })
        )

        div()(canvas, menu).toDom
    }

}
