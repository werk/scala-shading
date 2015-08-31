package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.Animation

sealed trait ViewState

object ViewState {
    case class ShowAnimation(animation : Animation) extends ViewState
    case object Pick0 extends ViewState
    case class Pick1(current : Animation) extends ViewState
    case class Pick2(current : Animation, argument : Option[Animation]) extends ViewState
}
