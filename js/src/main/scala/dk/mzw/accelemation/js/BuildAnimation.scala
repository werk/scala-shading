package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.js.BuildOrder._

class BuildAnimation(private var animationMap : Map[Id, Animation], private var effectMap : Map[Id, R => Animation => Animation], private var combinatorMap : Map[Id, Animation => Animation => Animation]) {

    def animations = animationMap.keys.toList
    def effects = effectMap.keys.toList
    def combinators = combinatorMap.keys.toList

    def apply(build : BuildOrder) : Animation = {
        val initial = animationMap(build.animationId)
        step(initial, build.actions)
    }

    private def step(currentAnimation : Animation, actions : Seq[Action]) : Animation = {
        actions match {
            case Seq() => currentAnimation
            case Effect(factor, effectId) +: rest =>
                val effect = effectMap(effectId)
                val nextAnimation = effect (factor) (currentAnimation)
                step(nextAnimation, rest)
            case Combine(animationId, combineId, flipped) +: rest =>
                val combine = combinatorMap(combineId)
                val animation = animationMap(animationId)
                val nextAnimation = flipped match {
                    case false => combine (currentAnimation) (animation)
                    case true => combine (animation) (currentAnimation)
                }
                step(nextAnimation, rest)
        }
    }

}
