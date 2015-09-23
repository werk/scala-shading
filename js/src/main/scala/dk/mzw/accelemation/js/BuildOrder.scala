package dk.mzw.accelemation.js

import dk.mzw.accelemation.js.BuildOrder._

case class BuildOrder(animationId : Id, actions : Seq[Action])

object BuildOrder {

    case class Id(userId : String, name : String)

    sealed trait Action
    case class Effect(factor : Double, effectId : Id) extends Action
    case class Combine(animationId : Id, effectId : Id, flipped : Boolean) extends Action


    def show(buildOrder : BuildOrder) : String = {
        val actions = buildOrder.actions.toList.flatMap(show)
        ("[initial]" :: buildOrder.animationId :: actions).map(_ + "\n").mkString
    }

    def show(action : Action) : List[String] = action match {
        case Effect(factor, effectId) => List("[effect]", s"factor = $factor", s"effectId = $effectId")
        case Combine(animationId, effectId, flipped) => List("[combine]", s"animationId = $animationId", s"flipped = $flipped")
    }


}
