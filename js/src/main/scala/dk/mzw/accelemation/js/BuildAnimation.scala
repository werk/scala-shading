package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.js.BuildOrder._

class BuildAnimation(var animations : Map[Id, Animation], var effects : Map[Id, R => Animation => Animation], var combinators : Map[Id, Animation => Animation => Animation]) {

    def apply(build : BuildOrder) : Animation = {
        val initial = animations(build.animationId)
        step(initial, build.actions)
    }

    private def step(currentAnimation : Animation, actions : Seq[Action]) : Animation = {
        actions match {
            case Seq() => currentAnimation
            case Effect(factor, effectId) +: rest =>
                val effect = effects(effectId)
                val nextAnimation = effect (factor) (currentAnimation)
                step(nextAnimation, rest)
            case Combine(animationId, combineId, flipped) +: rest =>
                val combine = combinators(combineId)
                val animation = animations(animationId)
                val nextAnimation = flipped match {
                    case false => combine (currentAnimation) (animation)
                    case true => combine (animation) (currentAnimation)
                }
                step(nextAnimation, rest)
        }
    }

}
