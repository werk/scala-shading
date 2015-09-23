package dk.mzw.accelemation.js

import dk.mzw.accelemation.js.BuildOrder._

import scala.collection.mutable.ListBuffer

case class BuildOrder(animationId : Id, actions : Seq[Action])

object BuildOrder {

    case class Id(userId : String, name : String)

    sealed trait Action
    case class Effect(factor : Double, effectId : Id) extends Action
    case class Combine(animationId : Id, effectId : Id, flipped : Boolean) extends Action

    def show(buildOrder : BuildOrder) : String = {
        show(Entry("initial", Seq("animationId" -> show(buildOrder.animationId)))) + buildOrder.actions.map(show).mkString
    }

    def show(action : Action) : String = action match {
        case Effect(factor, effectId) => show(Entry("effect", Seq("factor" -> factor.toString, "effectId" -> show(effectId))))
        case Combine(animationId, effectId, flipped) => show(Entry("combine", Seq("animationId" -> show(animationId), "effectId" -> show(effectId), "flipped" -> flipped.toString)))
    }

    def show(id : Id) = id.userId + "/" + id.name

    def show(entry : Entry) : String = {
        "[" + entry.text + "]\n" +
        entry.keyValues.map { case (k, v) =>
            k.replaceAll("[\\r\\n]+", " ") + " = " + v.replaceAll("[\\r\\n]+", " ") + "\n"
        }.mkString
    }

    def readId(text : String) = text.split("/") match {
        case Array(userId, name) => Id(userId, name)
        case _ => throw new RuntimeException("Can't parse ID: " + text)
    }

    def readBuildOrder(text : String) : BuildOrder = {
        val entries = readEntries(text)
        def expect[A](entry : Entry, keys : String*)(body : Seq[String] => A) = {
            val map = entry.keyValues.toMap
            val extra = map.keySet -- keys
            if(extra.nonEmpty) throw new RuntimeException("Unexpected keys: " + extra.mkString(", "))
            val values = keys.map(k => map.getOrElse(k, throw new RuntimeException("Expected key: " + k)))
            body(values)
        }
        if(!entries.headOption.exists(_.text == "initial")) throw new RuntimeException("File must start with [initial]")
        val initialAnimationId = expect(entries.head, "animationId") { case Seq(animationId) => readId(animationId) }
        val actions = for(entry <- entries.tail) yield entry match {
            case Entry("effect", keyValues) => expect(entry, "factor", "effectId") { case Seq(factor, effectId) =>
                Effect(factor.toDouble, readId(effectId))
            }
            case Entry("combine", keyValues) => expect(entry, "animationId", "effectId", "flipped") { case Seq(animationId, effectId, flipped) =>
                Combine(readId(animationId), readId(effectId), flipped.toBoolean)
            }
            case Entry(name, _) => throw new RuntimeException("Unexpected [" + name + "]")
        }
        BuildOrder(initialAnimationId, actions)
    }

    def readEntries(text : String) : Seq[Entry] = {
        val entries = new ListBuffer[Entry]()
        val keyValues = new ListBuffer[(String, String)]()
        var entryName : Option[String] = None
        def emitEntries() : Unit = {
            for(name <- entryName) {
                entries += Entry(name, keyValues.toList)
                keyValues.clear()
            }
        }
        for((line, lineNumber) <- text.lines.map(_.trim).zipWithIndex if !line.isEmpty) {
            if(line.startsWith("[")) {
                emitEntries()
                if(!line.endsWith("]")) throw new ParseException("Expected ']'", line, lineNumber + 1)
                entryName = Some(line.tail.dropRight(1).trim)
            } else if(entryName.isEmpty) {
                throw new ParseException("Expected '['", line, lineNumber + 1)
            } else {
                line.split("=", 2) match {
                    case Array(key, value) => keyValues += (key.trim -> value.trim)
                    case _ => throw new ParseException("Expected '='", line, lineNumber + 1)
                }
            }
        }
        emitEntries()
        entries.toList
    }

    case class ParseException(message : String, line : String, lineNumber : Int) extends RuntimeException(message + " at line " + lineNumber + ":" + line)

    case class Entry(text : String, keyValues : Seq[(String, String)])

}
