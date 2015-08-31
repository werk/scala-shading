package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.Animation
import org.scalajs.dom.Element

class AnimationWidget(animation : Animation) extends Widget {
    override def onResize(width: Int, height: Int): Unit = ???

    override def onDraw(): Unit = ???

    override val element: Element = ???
}
