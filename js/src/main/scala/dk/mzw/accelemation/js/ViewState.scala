package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.R
import dk.mzw.accelemation.js.BuildOrder.Id
import dk.mzw.accelemation.js.widget.{AnimationWidget, ListWidget, ParametersWidget, Widget}

sealed trait ViewState

object ViewState {
    type Flipped = Boolean

    case class ShowAnimation(build : BuildOrder) extends ViewState
    case class ShowList(listType : ListType, page : Int) extends ViewState
    case class ShowParameters(effect : R => BuildOrder) extends ViewState

    sealed trait ListType
    case object Pick0 extends ListType
    case class Pick1(current : BuildOrder) extends ListType
    case class Pick2(current : BuildOrder, argument : Option[(Id, Flipped)]) extends ListType

    def render(state : ViewState, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) : Widget = state match {
        case ShowAnimation(build) => new AnimationWidget(build, setViewState, buildAnimation)
        case ShowList(listType, page) => new ListWidget(listType, page, setViewState, buildAnimation)
        case ShowParameters(effect) => new ParametersWidget(effect, setViewState, buildAnimation)
    }
}

