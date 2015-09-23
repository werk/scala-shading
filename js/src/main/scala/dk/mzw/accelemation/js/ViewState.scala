package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.{R, Animation}
import dk.mzw.accelemation.js.BuildOrder.Id
import org.scalajs.dom

sealed trait ViewState

object ViewState {
    case class ShowAnimation(build : BuildOrder) extends ViewState
    case class ShowList(listType : ListType) extends ViewState
    case class ShowParameters(effect : R => Animation, buildBuild : Double => BuildOrder) extends ViewState

    sealed trait ListType
    case object Pick0 extends ListType
    case class Pick1(current : BuildOrder) extends ListType
    case class Pick2(current : BuildOrder, argument : Option[Id]) extends ListType

    def render(state : ViewState, setViewState : ViewState => Unit, buildAnimation : BuildAnimation) : Widget = state match {
        case ShowAnimation(build) => new AnimationWidget(build, setViewState, buildAnimation)
        case ShowList(listType) => new ListWidget(listType, setViewState, buildAnimation)
        case ShowParameters(effect, buildBuild) => new ParametersWidget(effect, setViewState, buildBuild)
    }
}

