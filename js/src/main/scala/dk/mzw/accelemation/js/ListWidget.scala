package dk.mzw.accelemation.js

import dk.mzw.accelemation.js.ViewState.ListType
import org.scalajs.dom.Element

class ListWidget(listType : ListType) extends Widget {
    override def onResize(width: Int, height: Int): Unit = ???

    override def onDraw(): Unit = ???

    override val element: Element = ???
}
