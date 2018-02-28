package dk.mzw.scalashading.internal

import scala.collection.mutable

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

    def toGraph[A](root : A, getChildren : A => Set[A]) : Map[A, Set[A]] = toGraph_(root, getChildren, Map())

    private def toGraph_[A](root : A, getChildren : A => Set[A], graph : Map[A, Set[A]]) : Map[A, Set[A]] = {
        val children = getChildren(root)
        var newGraph = graph
        newGraph += root -> children
        children.foreach { child =>
            if(!newGraph.contains(child)) newGraph = toGraph_(child, getChildren, newGraph)
        }
        newGraph
    }
}
