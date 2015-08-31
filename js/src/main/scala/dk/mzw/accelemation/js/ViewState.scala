package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.Animation
import org.scalajs.dom

sealed trait ViewState

object ViewState {
    case class ShowAnimation(animation : Animation) extends ViewState
    case class ShowList(listType : ListType) extends ViewState

    sealed trait ListType
    case object Pick0 extends ListType
    case class Pick1(current : Animation) extends ListType
    case class Pick2(current : Animation, argument : Option[Animation]) extends ListType

    def render(state : ViewState) : dom.Element = state match {
        case ShowAnimation(animation) => AnimationWidget(animation)
        case ShowList(listType) => ListWidget(listType)
    }
}

