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
        val actions = buildOrder.actions.toList.flatMap(show)
        ("[initial]" :: buildOrder.animationId :: actions).map(_ + "\n").mkString
    }

    def show(action : Action) : List[String] = action match {
        case Effect(factor, effectId) => List("[effect]", s"factor = $factor", s"effectId = $effectId")
        case Combine(animationId, effectId, flipped) => List("[combine]", s"animationId = $animationId", s"flipped = $flipped")
    }

    def show(buildOrder : Action) : String = {

    }

    def readEntries(text : String) : Seq[Entry] = {
        val entries = new ListBuffer[Entry]()
        val keyValues = new ListBuffer[(String, String)]()
        var entryName : Option[String] = None
        for((line, lineNumber) <- text.lines.map(_.trim).zipWithIndex if !line.isEmpty) {
            if(line.startsWith("[")) {
                for(name <- entryName) {
                    entries += Entry(name, keyValues.toList)
                    keyValues.clear()
                }
                if(!line.endsWith("]")) throw new ParseException("Expected ']'", line, lineNumber + 1)
                entryName = Some(line.tail.dropRight(1).trim)
            } else if(entryName.isEmpty) {
                throw new ParseException("Expected '['", line, lineNumber + 1)
            } else {
                line.split("=", 2) match {
                    case Array(key, value) => keyValues += (key -> value)
                    case _ => throw new ParseException("Expected '='", line, lineNumber + 1)
                }
            }
        }
        entries
    }

    case class ParseException(message : String, line : String, lineNumber : Int) extends RuntimeException(message + " at line " + lineNumber + ":" + line)

    case class Entry(text : String, keyValues : Seq[(String, String)])

}
