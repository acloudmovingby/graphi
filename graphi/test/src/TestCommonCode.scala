import utest.*

import graphi.{MapGraph, SimpleMapGraph, DirectedMapGraph}

object TestCommonCode {
	def testIsolates(graph: MapGraph[String]): Unit = {
		var g = graph
		assert(g.isolates.isEmpty)
		g = g.addNode("A")
		assert(g.isolates == Set("A"))
		g = g.addNode("B")
		assert(g.isolates == Set("A", "B"))
		g = g.addEdge("A", "B")
		assert(g.isolates.isEmpty)
		g = g.addNode("C")
		assert(g.isolates == Set("C"))
	}

	/**
	 * Produces all possible edges based on the nodes. In both directions, so returns (a -> b) as well as (b -> a)
	 */
	def allPossibleEdges[A](graph: MapGraph[A], includeSelfEdges: Boolean = false): Seq[(A, A)] = {
		for {
			node1 <- graph.nodes
			node2 <- if (includeSelfEdges) graph.nodes else graph.nodes.filter(_ != node1)
		} yield {
			(node1, node2)
		}
	}
}
