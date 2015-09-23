package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language.R
import dk.mzw.accelemation.js.BuildOrder._

case class BuildOrder(animationId : Id, actions : Seq[Action])

object BuildOrder {

    case class Id(userId : String, name : String)

    sealed trait Action
    case class Effect(factor : R, effectId : Id) extends Action
    case class Combine(animationId : Id, combineId : Id, flipped : Boolean) extends Action

    def show(buildOrder : BuildOrder) : String = {
        val actions = buildOrder.actions.flatMap(show)
        ("[initial]" +: buildOrder.animationId +: actions).map(_ + "\n").mkString
    }

    def show(action : Action) : Seq[String] = action match {
        case Effect(factor, effectId) => Seq("[effect]", s"effectId = $effectId", s"factor = $factor")
        case Combine(animationId, combineId, flipped) => Seq("[combine]", s"combineId = $combineId", s"animationId = $animationId", s"flipped = $flipped")
    }


}
