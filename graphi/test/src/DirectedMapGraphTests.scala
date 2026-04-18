import utest.*
import graphi.DirectedMapGraph
import TestCommonCode.{allPossibleEdges, testIsolates}

import scala.collection.immutable.HashMap

object DirectedMapGraphTests extends TestSuite {
	def tests = Tests {
		test("EmptyGraph") {
			val g = new DirectedMapGraph[String]()
			assert(g.nodeCount == 0)
			assert(g.edgeCount == 0)
		}
		test("OneNodeGraph") {
			var g = new DirectedMapGraph[String]()
			g = g.addNode("A")
			assert(g.nodeCount == 1)
			assert(g.edgeCount == 0)
		}
		test("TwoNodeGraph") {
			// add two nodes and add an edge between them
			var g = new DirectedMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addEdge("A", "B")
			assert(g.nodeCount == 2)
			assert(g.edgeCount == 1)
			assert(g.hasEdge("A", "B"))
			assert(!g.hasEdge("B", "A"))
			assert(!g.hasEdge("A", "C"))
			g = g.addEdge("B", "A")
			assert(g.edgeCount == 2)
			assert(g.hasEdge("B", "A"))
		}
		test("AddRedundantNodes") {
			// add two nodes with label "A" and ensure node count is still 1
			var g = new DirectedMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("A")
			assert(g.nodeCount == 1)
			assert(g.edgeCount == 0)
			assert(!g.hasEdge("A", "A"))
		}
		test("AddRedundantEdges") {
			// add two nodes and add the same edge twice, ensure edge count is still 1
			var g = new DirectedMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addEdge("A", "B")
			g = g.addEdge("A", "B")
			assert(g.nodeCount == 2)
			assert(g.edgeCount == 1)
			assert(g.hasEdge("A", "B"))
			assert(!g.hasEdge("B", "A"))
		}
		test("AddEdgeNonExistentNode") {
			// add an edge where one node doesn't exist, should throw NoSuchElementException
			var g = new DirectedMapGraph[String]()
			g = g.addNode("A")
			try {
				g = g.addEdge("A", "B")
				assert(false) // should not reach here
			} catch {
				case _: NoSuchElementException => assert(true) // expected
				case _: Throwable => assert(false) // unexpected
			}
			try {
				g = g.addEdge("B", "A")
				assert(false) // should not reach here
			} catch {
				case _: NoSuchElementException => assert(true) // expected
				case _: Throwable => assert(false) // unexpected
			}
			assert(g.nodeCount == 1)
			assert(g.edgeCount == 0)
		}
		test("getSuccessors") {
			// create a graph and test getSuccessors
			var g = new DirectedMapGraph[String]()
			for (node <- Seq("A", "B", "C")) {
				g = g.addNode(node)
			}
			g = g.addEdge("A", "B")
			g = g.addEdge("A", "C")
			g = g.addEdge("B", "C")
			val succA = g.getSuccessors("A")
			assert(succA == List("C", "B"))
			val succB = g.getSuccessors("B")
			assert(succB == List("C"))
			val succC = g.getSuccessors("C")
			assert(succC.isEmpty)
			// test non-existent node
			try {
				g.getSuccessors("D")
				assert(false) // should not reach here
			} catch {
				case _: NoSuchElementException => assert(true) // expected
				case _: Throwable => assert(false) // unexpected
			}
		}
		test("getPredecessors") {
			// create a graph and test getPredecessors
			var g = new DirectedMapGraph[String]()
			for (node <- Seq("A", "B", "C")) {
				g = g.addNode(node)
			}
			g = g.addEdge("A", "B")
			g = g.addEdge("A", "C")
			g = g.addEdge("B", "C")
			val predA = g.getPredecessors("A")
			assert(predA.isEmpty)
			val predB = g.getPredecessors("B")
			assert(predB == List("A"))
			val predC = g.getPredecessors("C")
			assert(predC == List("B", "A"))
			// test non-existent node
			try {
				g.getPredecessors("D")
				assert(false) // should not reach here
			} catch {
				case _: NoSuchElementException => assert(true) // expected
				case _: Throwable => assert(false) // unexpected
			}
		}
		test("djikstraTrivalGraphs") {
			// test djikstra on trivial graphs: empty graph, single node graph, two node graph with edge
			// empty graph
			var g = new DirectedMapGraph[String]()
			try {
				g.djikstra("A", "A")
				assert(false) // should not reach here
			} catch {
				case _: NoSuchElementException => assert(true) // expected
				case _: Throwable => assert(false) // unexpected
			}
			// single node graph
			g = g.addNode("A")
			val result1 = g.djikstra("A", "A")
			assert(result1.contains((List("A"), 0)))
			// two node graph without edge
			g = g.addNode("B")
			val result2 = g.djikstra("A", "B")
			assert(result2.isEmpty)
			// two node graph with edge
			g = g.addEdge("A", "B")
			val result3 = g.djikstra("A", "B")
			assert(result3.contains((List("A", "B"), 1)))
			// directed: no path from B to A
			val result4 = g.djikstra("B", "A")
			assert(result4.isEmpty)
		}
		test("djikstraLargerGraph") {
			// create a larger graph and test djikstra
			// Graph structure:
			// A -> B -> D
			// |   |    |
			// v   v    v
			// C   E -> F
			var g = new DirectedMapGraph[String]()
			for (node <- Seq("A", "B", "C", "D", "E", "F")) {
				g = g.addNode(node)
			}
			for ((from, to) <- Seq(("A", "B"), ("A", "C"), ("B", "C"), ("B", "D"), ("B", "E"), ("C", "E"), ("D", "F"), ("E", "F"))) {
				g = g.addEdge(from, to)
			}
			// test various paths
			val result1 = g.djikstra("A", "F")
			assert(result1.contains((List("A", "B", "E", "F"), 3)) || result1.contains((List("A", "C", "E", "F"), 3)))
			val result2 = g.djikstra("C", "D")
			assert(result2.isEmpty) // no path from C to D in directed graph
			val result3 = g.djikstra("A", "D")
			assert(result3.contains((List("A", "B", "D"), 2)))
			val result4 = g.djikstra("E", "A")
			assert(result4.isEmpty) // no path from E to A in directed graph
			// test path to non-existent node
			try {
				g.djikstra("A", "G")
				assert(false) // should not reach here
			} catch {
				case _: NoSuchElementException => assert(true) // expected
				case _: Throwable => assert(false) // unexpected
			}
		}
		test("toDotTrivial") {
			// test DOT output for trivial graphs
			// empty graph
			var g = new DirectedMapGraph[String]()
			val dot1 = g.toDot
			assert(dot1.trim == "digraph G {}")
			// single node graph
			g = g.addNode("A")
			val dot2 = g.toDot
			assert(dot2.trim == "digraph G {\n  \"A\";\n}")
			// two node graph with one edge
			g = g.addNode("B")
			g = g.addEdge("A", "B")
			val dot3 = g.toDot
			val expectedLines = Set(
				"digraph G {",
				""""A" -> "B";""",
				"}"
			)
			val dotLines = dot3.split("\n").map(_.trim).toSet
			assert(dotLines == expectedLines)
		}

		test("toDot") {
			// create a graph and test the DOT output
			var g = new DirectedMapGraph[String]()
			for (node <- Seq("A", "B", "C", "D")) {
				g = g.addNode(node)
			}
			g = g.addEdge("A", "B")
			g = g.addEdge("B", "C")
			g = g.addEdge("A", "C")
			val dot = g.toDot
			val expectedLines = Set(
				"digraph G {",
				""""A" -> "B";""",
				""""A" -> "C";""",
				""""B" -> "C";""",
				""""D";""",
				"}"
			)
			val dotLines = dot.split("\n").map(_.trim).toSet
			assert(dotLines == expectedLines)
		}

		test("isolates") {
			testIsolates(new DirectedMapGraph[String]())
		}
		test("removeEdge") {
			// Setup: create a graph with two nodes and an edge
			var g = new DirectedMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addEdge("A", "B")
			assert(g.edgeCount == 1)
			assert(g.hasEdge("A", "B"))

			// Remove existing edge
			g = g.removeEdge("A", "B")
			assert(g.edgeCount == 0)
			assert(!g.hasEdge("A", "B"))

			// Remove non-existent edge (should be a no-op)
			g = g.removeEdge("A", "B")
			assert(g.edgeCount == 0)
			assert(!g.hasEdge("A", "B"))

			// Remove edge where one node does not exist (should throw)
			try {
				g.removeEdge("A", "C")
				assert(false)
			} catch {
				case _: NoSuchElementException => assert(true)
				case _: Throwable => assert(false)
			}
		}
		test("removeSingleDirectionOfBidirectionalEdge") {
			// Setup: create a graph with two nodes and bidirectional edges
			var g = new DirectedMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addEdge("A", "B")
			g = g.addEdge("B", "A")
			assert(g.edgeCount == 2)
			assert(g.hasEdge("A", "B"))
			assert(g.hasEdge("B", "A"))

			// Remove only one direction
			g = g.removeEdge("A", "B")
			assert(g.edgeCount == 1)
			assert(!g.hasEdge("A", "B"))
			assert(g.hasEdge("B", "A"))

			// Remove the other direction
			g = g.removeEdge("B", "A")
			assert(g.edgeCount == 0)
			assert(!g.hasEdge("A", "B"))
			assert(!g.hasEdge("B", "A"))
		}

		test("uniqueEdgesWithDirection") {
			// create a graph with both bidirectional edges and unidirectional edges.
			var g = new DirectedMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addNode("C")
			g = g.addEdge("A", "B")
			g = g.addEdge("B", "A") // bidirectional between A and B
			g = g.addEdge("A", "C") // unidirectional from A to C
			val edges = g.uniqueEdgesWithDirection
			assert(edges.size == 2)
			assert(edges.contains((("A", "B"), true)) ^ edges.contains((("B", "A"), true))) // bidirectional edge
			assert(edges.contains((("A", "C"), false))) // unidirectional edge
		}

		test("numComponents - trivial") {
			val g = new DirectedMapGraph[String]()
			assert(g.numComponents() == 0)
		}

		test("numComponents - single node") {
			val g = new DirectedMapGraph[String]().addNode("A")
			assert(g.numComponents() == 1)
		}

		test("numComponents - two nodes, one edge") {
			val g = new DirectedMapGraph[String]().addNode("A").addNode("B").addEdge("A", "B")
			assert(g.numComponents() == 1)
			val g2 = new DirectedMapGraph[String]().addNode("A").addNode("B").addEdge("B", "A")
			assert(g2.numComponents() == 1)
		}

		test("numComponents - two nodes, two edges") {
			val g = new DirectedMapGraph[String]().addNode("A").addNode("B").addEdge("A", "B").addEdge("B", "A")
			assert(g.numComponents() == 1)
		}

		test("numComponents - three nodes, three components") {
			val g = new DirectedMapGraph[String]().addNode("A").addNode("B").addNode("C")
			assert(g.numComponents() == 3)
		}

		test("numComponents - three nodes, one edge, means two components") {
			val g = new DirectedMapGraph[String]().addNode("A").addNode("B")
			for {
				edge <- g.nodes.combinations(2).flatMap(_.permutations.toSeq) // all possible edges
			} yield {
				val g2 = g.addEdge(edge(0), edge(1))
				if (g2.numComponents() != 2) println(s"Uh, this didn't work: ${g2.getEdges}")
				assert(g2.numComponents() == 2)
				// now add the other direction, shouldn't change component count
				val g3 = g2.addEdge(edge(1), edge(0))
				if (g3.numComponents() != 2) println(s"Uh, this didn't work: ${g3.getEdges}")
				assert(g3.numComponents() == 2)
			}
		}

		test("numComponents - three nodes, one UNIdirectional edge, means two components") {
			val g = new DirectedMapGraph[String]().addNode("A").addNode("B").addNode("C")

			for {
				node1 <- g.nodes
				node2 <- g.nodes
			} yield {
				if (node1 != node2) {
					val g2 = g.addEdge(node1, node2)
					assert(g2.numComponents() == 2)
				}
			}
		}

		test("numComponents - three nodes, one BIdirectional edge, means two components") {
			val g = new DirectedMapGraph[String]().addNode("A").addNode("B").addNode("C")

			for {
				node1 <- g.nodes
				node2 <- g.nodes
			} yield {
				if (node1 != node2) {
					val g2 = g.addEdge(node1, node2).addEdge(node2, node1) // the one line different from test above
					assert(g2.numComponents() == 2)
				}
			}
		}

		test("numComponents - three nodes, two UNIdirectional edges, means one component") {
			val g = new DirectedMapGraph[String]().addNode("A").addNode("B").addNode("C")
			for {
				edge1 <- allPossibleEdges(g)
				edge2 <- allPossibleEdges(g).filter(e => e != edge1 && e != (edge1._2, edge1._1))
			} yield {
				val g2 = g.addEdge(edge1._1, edge1._2).addEdge(edge2._1, edge2._2)
				val result = g2.numComponents()
				if (result != 1) println(s"Didn't work with edges: $edge1, $edge2, component count=${result}")
				assert(result == 1)
			}
		}
	}
}