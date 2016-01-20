package dk.mzw.accelemation.js

import dk.mzw.accelemation.Language._
import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.js.BuildOrder._
import dk.mzw.accelemation.js.debug.{LongTime, ExecutionTime}

class BuildAnimation(
    var effectMap : Map[Id, R => Animation => Animation],
    var combinatorMap : Map[Id, Animation => Animation => Animation],
    var effectName: Map[Id, String],
    var compinatorName: Map[Id, String]
) {

    case class Cache(name : String, glsl : String, call : Animation, dependencies : Set[Id])

    private var compiled = Map[Id, Cache]()
    private var sortedAnimations = List[Id]()

    def animationName(id : Id) : String = compiled(id).name

    def addPrelude(map : Map[Id, Animation]): Unit = {
        val more = map.map{case (id, animation) =>
            //val functionName = safeName(id.userId + "_" + id.key)
            val functionName = safeName(id, id.key)
            id -> Cache(id.key, ToGlsl.function(functionName, animation), ToGlsl.provided(functionName), Set())
        }
        compiled ++= more
        resort()
    }

    def addSavedBuildOrders(buildOrders : Seq[BuildOrder]): Unit = {
        val idMap = buildOrders.map(b => b.saved.get.id -> b).toMap
        val graph = idMap.mapValues(dependencies) ++ compiled.mapValues(_.dependencies)
        //println("graph:")
        def name(id : Id) : String = idMap.get(id).map(_.saved.get.name).getOrElse(compiled(id).name)
        //graph.foreach{case (id, ids) => println(s"  ${name(id)} -> ${ids.map(name).mkString(", ")}")}
        val sorted = Topological[Id](graph).filter(idMap.contains)

        //println(s"Sorted depencendied (addSavedBuildOrders): ")
        //sorted.foreach(id => println(s"  ${idMap(id).saved.get.name} (${id.key}) -> ${graph(id).map(name).mkString(", ")}"))

        sorted.foreach{id =>
            val buildOrder = idMap(id)
            val info = buildOrder.saved.get
            //println(s"Caching ${id.userId} / ${id.key}/ ${info.name}")
            //val functionName = safeName(id.userId + "_" + id.key + "_" + info.name)
            val functionName = safeName(id, info.name)
            val animation = makeAnimation(buildOrder)
            val dependencies = BuildOrder.dependencies(buildOrder)
            val cache = Cache(info.name, ToGlsl.function(functionName, animation), ToGlsl.provided(functionName), dependencies)
            compiled += id -> cache
        }
        resort()
    }

    var names = Map[Id, String]()

    private def safeName(id : Id, name : String) : String = {
        names.getOrElse(id, {
            val preferedName = name.trim.replaceAll("[^a-zA-Z0-9]+", "_")
            val takenNames = names.values.toSet
            if(!takenNames.contains(preferedName)) {
                names += id -> preferedName
                preferedName
            } else {
                safeName(id, name+"_I")
            }

        })
    }

    def dependenciesTransitive(buildOrder : BuildOrder) : Set[Id] = {
        def dep(id : Id) : Set[Id] = {
            compiled(id).dependencies.flatMap(dep) + id
        }
        dependencies(buildOrder).flatMap(dep)
    }

    def toGlsl(buildOrder : BuildOrder) : String = {
        val used = dependenciesTransitive(buildOrder)
        //println(s"Used for ${buildOrder.saved}: ${used.mkString(",")}")
        val functions = sortedAnimations.filter(used).map(compiled(_).glsl).mkString
        val animation = makeAnimation(buildOrder)
        val (glsl, time) = ExecutionTime(ToGlsl(animation, functions))
        println(s"To GLSL: ${glsl.length / 1000} KB in ${LongTime.pretty(time)}")
        //println(glsl)
        //println()
        glsl
    }

    private def resort(): Unit = {
        val graph = compiled.mapValues(_.dependencies)
        sortedAnimations = Topological[Id](graph)
        //println(s"Sorted depencendied: ")
        //sortedAnimations.foreach(id => println(s"   ${id.userId} / ${compiled(id).name}"))
    }

    def animations = sortedAnimations
    def effects = effectMap.keys.toList
    def combinators = combinatorMap.keys.toList

    private def makeAnimation(build : BuildOrder) : Animation = {
        val initial = compiled(build.animationId).call
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
                val animation = compiled(animationId).call
                val nextAnimation = flipped match {
                    case false => combine (currentAnimation) (animation)
                    case true => combine (animation) (currentAnimation)
                }
                step(nextAnimation, rest)
        }
    }

}
