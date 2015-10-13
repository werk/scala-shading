package dk.mzw.accelemation.js.widget

import org.scalajs.dom
import org.scalajs.dom.document


object Gui {

    def middle(styles : (String, String)*)(children : DomElement*) : DomElement = {
        val td = tag("td")(
            "text-align" -> "center",
            "vertical-align" -> "middle",
            "height" -> "100%",
            "width" -> "100%",
            "font-weight" -> "bold"
        )(children : _*)
        val tr = tag("tr")()(td)
        tag("table")()(tr)
    }

    def fullSize(margin : String = "0") = div(
        "position" -> "absolute",
        "top" -> margin,
        "left" -> margin,
        "bottom" -> margin,
        "right" -> margin
    )

    def roundButton(content : DomElement, color : String, click : => Unit) = {
        val td = tag("td")(
            "text-align" -> "center",
            "vertical-align" -> "middle",
            "height" -> "100%",
            "width" -> "100%",
            "color" -> "white",
            "font-weight" -> "bold"
        )(content)
        val tr = tag("tr")()(td)
        val table = tag("table")(
            "height" -> "100px",
            "width" -> "100px",
            "margin-left" -> "5px",
            "margin-right" -> "5px",
            "border-radius" -> "100%",
            "background-color" -> color,
            "cursor" -> "pointer"
        )(tr).click(_ => click)
        inlineBlock()(table)
    }

    def tag(tagName : String, attributes : (String, String)*)(styles : (String, String)*) = RichElement(tagName, attributes, Seq(), Seq(), styles, None, None, None, None)
    def div(styles : (String, String)*) = tag("div")(styles : _*)
    def span(styles : (String, String)*) = tag("span")(styles : _*)
    def inlineBlock(styles : (String, String)*) = div(styles : _*).style("display" -> "inline-block")

    implicit def fromText(text : String) : DomElement = TextElement(text)
    implicit def fromElement(node : dom.Element) : DomElement = NodeElement(node)

    sealed trait DomElement {
        def toDom : dom.Node
    }
    
    case class NodeElement(element : dom.Element) extends DomElement {
        override def toDom : dom.Element = element
    }
    
    case class TextElement(text : String) extends DomElement {
        override def toDom: dom.Node = document.createTextNode(text)
    }
    
    case class RichElement(tag : String, attributes : Seq[(String, String)], children : Seq[DomElement], classes : Seq[String], styles : Seq[(String, String)], onClicks : Option[dom.MouseEvent => Unit], onMouseMoves : Option[dom.MouseEvent => Unit], onMouseDowns : Option[dom.MouseEvent => Unit], onChanges : Option[dom.Event => Unit]) extends DomElement {

        def click(handler : dom.MouseEvent => Unit) : RichElement = copy(onClicks = Some(handler))
        def mouseMove(handler : dom.MouseEvent => Unit) : RichElement = copy(onMouseMoves = Some(handler))
        def mouseDown(handler : dom.MouseEvent => Unit) : RichElement = copy(onMouseDowns = Some(handler))
        def change(handler : dom.Event => Unit) : RichElement = copy(onChanges = Some(handler))
        def apply(children : DomElement*) : RichElement = copy(children = this.children ++ children)
        def flatAppend(children : Option[DomElement]*) = apply(children.flatten : _*)
        def style(styles : (String, String)*) = copy(styles = this.styles ++ styles)
        def addClasses(classes : String*) = copy(classes = this.classes ++ classes)

        override def toDom : dom.Element = {
            val e = document.createElement(tag)
            for((k, v) <- attributes) e.setAttribute(k, v)
            if(styles.nonEmpty) e.setAttribute("style", styles.map{case (k, v) => s"$k: $v; "}.mkString)
            if(classes.nonEmpty) e.setAttribute("class", classes.mkString(" "))
            if(onClicks.nonEmpty) {
                e.addEventListener("click", { e : dom.MouseEvent =>
                    onClicks.foreach(_(e))
                })
            }
            if(onMouseMoves.nonEmpty) {
                e.addEventListener("mousemove", { e : dom.MouseEvent =>
                    onMouseMoves.foreach(_(e))
                })
            }
            if(onMouseDowns.nonEmpty) {
                e.addEventListener("mousedown", { e : dom.MouseEvent =>
                    onMouseDowns.foreach(_(e))
                })
            }
            if(onChanges.nonEmpty) {
                e.addEventListener("change", { e : dom.Event =>
                    onChanges.foreach(_(e))
                })
            }
            children.foreach(c => e.appendChild(c.toDom))
            e
        }
    }
}
