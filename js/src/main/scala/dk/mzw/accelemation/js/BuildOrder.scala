package dk.mzw.accelemation.js

import dk.mzw.accelemation.Internal.Constant
import dk.mzw.accelemation.Language.{Term, R}
import dk.mzw.accelemation.js.BuildOrder._

import scala.collection.mutable.ListBuffer

case class SavedInfo(id : Id, name : String)
case class BuildOrder(saved : Option[SavedInfo], animationId : Id, actions : Seq[Action])

object BuildOrder {

    case class Id(userId : String, key : String)

    sealed trait Action
    case class Effect(factor : R, effectId : Id) extends Action
    case class Combine(animationId : Id, effectId : Id, flipped : Boolean) extends Action

    def show(buildOrder : BuildOrder) : String = {
        val meta = buildOrder.saved.map(s => show(Entry("meta", Seq("id" -> show(s.id), "name" -> s.name)))).getOrElse("")
        val initial = show(Entry("initial", Seq("animationId" -> show(buildOrder.animationId))))
        meta + initial + buildOrder.actions.map(show).mkString
    }

    def show(action : Action) : String = action match {
        case Effect(Term(Constant(factor)), effectId) => show(Entry("effect", Seq("factor" -> factor.toString, "effectId" -> show(effectId))))
        case Effect(_, effectId) => throw new RuntimeException("Unrecognized effect parameter for effect ID: " + effectId)
        case Combine(animationId, combineId, flipped) => show(Entry("combine", Seq("animationId" -> show(animationId), "combineId" -> show(combineId), "flipped" -> flipped.toString)))
    }

    def show(id : Id) = id.userId + "/" + id.key

    def show(entry : Entry) : String = {
        "[" + entry.text.replaceAll("[\\r\\n\\]]+", " ") + "]\n" +
        entry.keyValues.map { case (k, v) =>
            k.replaceAll("[\\r\\n=]+", " ") + " = " + v.replaceAll("[\\r\\n]+", " ") + "\n"
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
        if(!entries.headOption.exists(_.text == "meta")) throw new RuntimeException("File must start with [meta]")
        if(!entries.tail.headOption.exists(_.text == "initial")) throw new RuntimeException("File must have [initial] after [meta]")
        val savedInfo = expect(entries.head, "id", "name") { case Seq(key, name) => SavedInfo(readId(key), name) }
        val initialAnimationId = expect(entries.tail.head, "animationId") { case Seq(animationId) => readId(animationId) }
        val actions = for(entry <- entries.tail.tail) yield entry match {
            case Entry("effect", keyValues) => expect(entry, "factor", "effectId") { case Seq(factor, effectId) =>
                Effect(factor.toDouble, readId(effectId))
            }
            case Entry("combine", keyValues) => expect(entry, "animationId", "combineId", "flipped") { case Seq(animationId, combineId, flipped) =>
                Combine(readId(animationId), readId(combineId), flipped.toBoolean)
            }
            case Entry(name, _) => throw new RuntimeException("Unexpected [" + name + "]")
        }
        BuildOrder(Some(savedInfo), initialAnimationId, actions)
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
