package graphi.test

import utest.*

import graphi.{SimpleMapGraph, DirectedMapGraph}

object TestCommonCode {
	def testIsolates(graph: SimpleMapGraph[String] | DirectedMapGraph[String]): Unit = {
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
}
