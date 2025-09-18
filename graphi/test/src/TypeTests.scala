package graphi.test

import utest.*

import graphi.SimpleMapGraph
import graphi.algorithms.GraphType.{isNullGraph, isTrivial, isCycleGraph}

object TypeTests extends TestSuite {
	def tests = Tests {
		test("isNullGraph") {
			val g = new SimpleMapGraph[String]()
			assert(isNullGraph(g))
			val g2 = g.addNode("A")
			assert(!isNullGraph(g2))
		}
		test("isTrivial") {
			val g = new SimpleMapGraph[String]()
			assert(!isTrivial(g))
			val g2 = g.addNode("A")
			assert(isTrivial(g2))
			val g3 = g2.addNode("B")
			assert(!isTrivial(g3))
			val g4 = g2.addEdge("A", "A") // self-loop not currently allowed
			assert(isTrivial(g4))
		}
		test("isCycleGraph") {
			val g = new SimpleMapGraph[String]()
			assert(!isCycleGraph(g))
			val g2 = g.addNode("A")
			assert(!isCycleGraph(g2))
			val g3 = g2.addNode("B").addNode("C")
			assert(!isCycleGraph(g3))
			val g4 = g3.addEdge("A", "B").addEdge("B", "C").addEdge("C", "A")
			assert(isCycleGraph(g4))
			val g5 = g4.addNode("D")
			assert(!isCycleGraph(g5))
			val g6 = g5.addEdge("C", "D").addEdge("D", "A")
			assert(!isCycleGraph(g6))

			val g7 = new SimpleMapGraph[Int]()
				.addNode(1)
				.addNode(2)
				.addNode(3)
				.addNode(4)
				.addEdge(1, 2)
				.addEdge(2, 3)
				.addEdge(3, 4)
				.addEdge(4, 1)
			assert(isCycleGraph(g7))
		}
	}
}