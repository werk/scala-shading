package dk.mzw.accelemation.js

import scala.collection.mutable
import scala.util.Random

object Topological {

    def apply[A](graph : Map[A, Set[A]]) : List[A] = {
        val depths = mutable.Map[A, Int]()

        def walk(node : A, depth : Int): Unit = {
            depths.get(node) match {
                 case Some(d) if d >= depth => // Stop
                 case _ =>
                    depths.put(node, depth)
                    graph(node).foreach(walk(_, depth + 1))
             }
        }

        val hasIncomming = graph.values.flatten.toSet
        val hasNoIncomming = graph.keySet -- hasIncomming

        hasNoIncomming.foreach(walk(_, 1))

        graph.keys.toList.sortBy(-depths(_))
    }

    def main(arguments : Array[String]): Unit = {

        val list = List[(String, Set[String])]("blue" -> Set(), "redball" -> Set("ball", "red"), "ball" -> Set(), "timelens" -> Set("ball"), "balls" -> Set("redball", "blueball"), "blueball" -> Set("ball", "blue"), "red" -> Set())
        val graph = new Random().shuffle(list).toMap
        val sorted = apply(graph)
        sorted.foreach(println)

    }

}
