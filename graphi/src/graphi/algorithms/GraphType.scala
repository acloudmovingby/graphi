package graphi.algorithms

import graphi.{DirectedMapGraph, MapGraph, SimpleMapGraph}

/** Helper functions to determine the class or type of graph. */
object GraphType {
	def isTrivial(g: MapGraph[?, ?]): Boolean = g.nodeCount == 1 && g.edgeCount == 0

	def isNullGraph(g: MapGraph[?, ?]): Boolean = g.nodeCount == 0 && g.edgeCount == 0

	def isCycleGraph(g: MapGraph[?, ?]): Boolean = g match {
		case g: SimpleMapGraph[?] =>
			g.nodeCount >= 3
			&& g.nodeCount == g.edgeCount
			&& g.getNodes.forall(n => g.getDegree(n) == 2)
		case g: DirectedMapGraph[?] =>
			g.nodeCount >= 3
			&& g.nodeCount == g.edgeCount
			&& g.getNodes.forall(n => g.getOutDegree(n) == 1 && g.getInDegree(n) == 1)
		case _ => ???
	}
}
