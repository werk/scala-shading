package dk.mzw.accelemation.js.widget

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.Element


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

    def tag(tagName : String)(styles : (String, String)*) = RichElement(tagName, Seq(), Seq(), styles, None, None, None)
    def div(styles : (String, String)*) = tag("div")(styles : _*)
    def span(styles : (String, String)*) = tag("span")(styles : _*)
    def inlineBlock(styles : (String, String)*) = div(styles : _*).style("display" -> "inline-block")

    implicit def fromText(text : String) : DomElement = TextElement(text)
    implicit def fromElement(e : Element) : DomElement = NodeElement(e)
    implicit def fromElement(e : dom.Element) : DomElement = fromElement(e.asInstanceOf[Element])

    trait DomElement {
        def toDom : Element
    }
    
    case class NodeElement(element : Element) extends DomElement {
        override def toDom : Element = element
    }
    
    case class TextElement(text : String) extends DomElement {
        override def toDom: Element = {
            val span = document.createElement("span")
            span.appendChild(document.createTextNode(text))
            span.asInstanceOf[Element]
        }

    }
    
    case class RichElement(tag : String, children : Seq[DomElement], classes : Seq[String], styles : Seq[(String, String)], onClicks : Option[dom.MouseEvent => Unit], onMouseMoves : Option[dom.MouseEvent => Unit], onMouseDowns : Option[dom.MouseEvent => Unit]) extends DomElement {

        def click(handler : dom.MouseEvent => Unit) : RichElement = copy(onClicks = Some(handler))
        def mouseMove(handler : dom.MouseEvent => Unit) : RichElement = copy(onMouseMoves = Some(handler))
        def mouseDown(handler : dom.MouseEvent => Unit) : RichElement = copy(onMouseDowns = Some(handler))
        def apply(children : DomElement*) : RichElement = copy(children = this.children ++ children)
        def flatAppend(children : Option[DomElement]*) = apply(children.flatten : _*)
        def style(styles : (String, String)*) = copy(styles = this.styles ++ styles)
        def addClasses(classes : String*) = copy(classes = this.classes ++ classes)

        override def toDom : Element = {
            val e = document.createElement(tag).asInstanceOf[Element]
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
            children.foreach(c => e.appendChild(c.toDom))
            e
        }
    }
}
