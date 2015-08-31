package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.{R, Animation}
import org.scalajs.dom

sealed trait ViewState

object ViewState {
    case class ShowAnimation(animation : Animation) extends ViewState
    case class ShowList(listType : ListType) extends ViewState
    case class ShowParameters(effect : R => Animation) extends ViewState

    sealed trait ListType
    case object Pick0 extends ListType
    case class Pick1(current : Animation) extends ListType
    case class Pick2(current : Animation, argument : Option[Animation]) extends ListType

    def render(state : ViewState, setViewState : ViewState => Unit) : Widget = state match {
        case ShowAnimation(animation) => new AnimationWidget(animation, setViewState)
        case ShowList(listType) => new ListWidget(listType, setViewState)
        case ShowParameters(effect) => new ParametersWidget(effect, setViewState)
    }
}

