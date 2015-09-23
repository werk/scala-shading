package dk.mzw.accelemation.js

import dk.mzw.accelemation.js.BuildOrder._

case class BuildOrder(animationId : Id, actions : Seq[Action])

object BuildOrder {

    case class Id(userId : String, name : String)

    sealed trait Action
    case class Effect(factor : Double, effectId : Id) extends Action
    case class Combine(animationId : Id, effectId : Id, flipped : Boolean) extends Action


    def show(buildOrder : BuildOrder) : String = {

    }

    def show(buildOrder : Action) : String = {

    }


}
