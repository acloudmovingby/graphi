import utest.*
import graphi.SimpleMapGraph
import TestCommonCode.testIsolates

import scala.collection.immutable.HashMap

object SimpleMapGraphTests extends TestSuite {
	def tests = Tests {
		test("EmptyGraph") {
			val g = new SimpleMapGraph[String]()
			assert(g.nodeCount == 0)
			assert(g.edgeCount == 0)
		}
		test("OneNodeGraph") {
			var g = new SimpleMapGraph[String]()
			g = g.addNode("A")
			assert(g.nodeCount == 1)
			assert(g.edgeCount == 0)
		}
		test("TwoNodeGraph") {
			// add two nodes and add an edge between them
			var g = new SimpleMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addEdge("A", "B")
			assert(g.nodeCount == 2)
			assert(g.edgeCount == 1)
			assert(g.hasEdge("A", "B"))
			assert(g.hasEdge("B", "A"))
			assert(!g.hasEdge("A", "C"))
			// adding the reverse edge again should not change edge count (since undirected, so implied bidirectional)
			g = g.addEdge("B", "A")
			assert(g.edgeCount == 1)
			assert(g.hasEdge("B", "A"))
		}
		test("AddRedundantNodes") {
			// add two nodes with label "A" and ensure node count is still 1
			var g = new SimpleMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("A")
			assert(g.nodeCount == 1)
			assert(g.edgeCount == 0)
			assert(!g.hasEdge("A", "A"))
		}
		test("AddRedundantEdges") {
			// add two nodes and add the same edge twice, ensure edge count is still 1
			var g = new SimpleMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addEdge("A", "B")
			g = g.addEdge("A", "B")
			assert(g.nodeCount == 2)
			assert(g.edgeCount == 1)
			assert(g.hasEdge("A", "B"))
			assert(g.hasEdge("B", "A"))
		}
		test("AddEdgeNonExistentNode") {
			// add an edge where one node doesn't exist, should throw NoSuchElementException
			var g = new SimpleMapGraph[String]()
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
		test("getNeighbors") {
			// add three nodes and two edges, then check neighbors
			var g = new SimpleMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addNode("C")
			// check neighbors of unconnected nodes
			assert(g.getNeighbors("A").isEmpty)
			assert(g.getNeighbors("B").isEmpty)
			assert(g.getNeighbors("C").isEmpty)
			// add edges
			g = g.addEdge("A", "B")
			g = g.addEdge("A", "C")
			val neighborsA = g.getNeighbors("A")
			val neighborsB = g.getNeighbors("B")
			val neighborsC = g.getNeighbors("C")
			assert(neighborsA == List("C", "B"))
			assert(neighborsB == List("A"))
			assert(neighborsC == List("A"))
			try {
				g.getNeighbors("D")
				assert(false) // should not reach here
			} catch {
				case _: NoSuchElementException => assert(true) // expected
				case _: Throwable => assert(false) // unexpected
			}
		}
		test("djikstraTrivalGraphs") {
			// test djikstra on trivial graphs: empty graph, single node graph, two node graph with edge
			// empty graph
			var g = new SimpleMapGraph[String]()
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
		}
		test("djikstraLargerGraph") {
			// create a larger graph and test djikstra
			// Graph structure:
			// A -- B -- D
			// |  / |    |
			// C -- E -- F
			var g = new SimpleMapGraph[String]()
			for (node <- Seq("A", "B", "C", "D", "E", "F")) {
				g = g.addNode(node)
			}
			for ((from, to) <- Seq(("A", "B"), ("A", "C"), ("B", "C"), ("B", "D"), ("B", "E"), ("C", "E"), ("D", "F"), ("E", "F"))) {
				g = g.addEdge(from, to)
			}
			// test various paths
			// multiple valid shortest paths may exist, so check for all possibilities
			val result1 = g.djikstra("A", "F")
			assert(result1.contains((List("A", "B", "E", "F"), 3)) || result1.contains((List("A", "C", "E", "F"), 3)))
			val result2 = g.djikstra("C", "D")
			assert(result2.contains((List("C", "B", "D"), 2)))
			val result3 = g.djikstra("A", "D")
			assert(result3.contains((List("A", "B", "D"), 2)))
			val result4 = g.djikstra("E", "A")
			assert(result4.contains((List("E", "B", "A"), 2)) || result4.contains((List("E", "C", "A"), 2)))
			// test path to non-existent node
			try {
				g.djikstra("A", "G")
				assert(false) // should not reach here
			} catch {
				case _: NoSuchElementException => assert(true) // expected
				case _: Throwable => assert(false) // unexpected
			}
		}

		test("toDot") {
			// create a simple graph and test the DOT output
			var g = new SimpleMapGraph[String]()
			for (node <- Seq("A", "B", "C", "D")) {
				g = g.addNode(node)
			}
			g = g.addEdge("A", "B")
			g = g.addEdge("B", "C")
			val dot = g.toDot
			val expectedLines = List(
				"graph G {",
				""""B" -- "C";""",
				""""A" -- "B";""",
				""""D";""",
				"}"
			)
			val dotLines = dot.split("\n").map(_.trim).toList
			assert(dotLines == expectedLines)
		}

		test("isolates") {
			testIsolates(new SimpleMapGraph[String]())
		}
		test("removeEdge") {
			// Setup: create a graph with two nodes and an edge
			var g = new SimpleMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addEdge("A", "B")
			assert(g.edgeCount == 1)
			assert(g.hasEdge("A", "B"))
			assert(g.hasEdge("B", "A"))

			// Remove existing edge
			g = g.removeEdge("A", "B")
			assert(g.edgeCount == 0)
			assert(!g.hasEdge("A", "B"))
			assert(!g.hasEdge("B", "A"))

			// Remove non-existent edge (should be a no-op)
			g = g.removeEdge("A", "B")
			assert(g.edgeCount == 0)
			assert(!g.hasEdge("A", "B"))
			assert(!g.hasEdge("B", "A"))

			// Remove edge where one node does not exist (should throw)
			try {
				g.removeEdge("A", "C")
				assert(false)
			} catch {
				case _: NoSuchElementException => assert(true)
				case _: Throwable => assert(false)
			}
		}

		test("depth first search") {
			try {
				val g = new SimpleMapGraph[Int]()
				g.depthFirstSearch(1)
				assert(false) //
			} catch {
				case _: NoSuchElementException => assert(true)
				case _: Throwable => assert(false)
			}

			val g2 = new SimpleMapGraph[String]()
				.addNode("A")
				.addNode("B")
				.addNode("C")
				.addNode("D")
				.addEdge("A", "B")
				.addEdge("B", "C")
				.addEdge("C", "D")
				.addEdge("A", "D")

			val result = g2.depthFirstSearch("A")
			println(s"result=$result")
			assert(result.size == 4)
			assert(result.toSet.size == 4) // assert all unique
			assert(result.toSet == g2.nodes.toSet) // assert all nodes visited

			val g3 = new SimpleMapGraph[Int](HashMap(0 -> List(1, 2, 3), 5 -> List(), 1 -> List(), 6 -> List(), 2 -> List(6, 7), 7 -> List(), 3 -> List(4, 5), 8 -> List(), 4 -> List(8)))
			val result3 = g3.depthFirstSearch(0).toIndexedSeq
			assert(result3.size == 9)
			assert(result3.toSet.size == 9) // assert all unique
		}

		test("uh, dfs but like to demonstrate things") {
			val g = new SimpleMapGraph[Int](HashMap(
				0 -> List(1, 2),
				1 -> List(0, 3, 4),
				2 -> List(0, 5, 6),
				3 -> List(1, 7, 8),
				4 -> List(1, 9, 10),
				5 -> List(2, 11, 12),
				6 -> List(2, 14, 13),
				7 -> List(3),
				8 -> List(3),
				9 -> List(4),
				10 -> List(4),
				11 -> List(5),
				12 -> List(5),
				13 -> List(6),
				14 -> List(6)
			))

			println("TEST TEST TEST")
			println(g.depthFirstSearch(0))
		}

		test("getInNeighbors - trivial graph, no edges") {
			var g = new SimpleMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addNode("C")
			assert(g.getInNeighbors("A").isEmpty)
			assert(g.getInNeighbors("B").isEmpty)
			assert(g.getInNeighbors("C").isEmpty)
		}

		test("getInNeighbors - simple graph") {
			var g = new SimpleMapGraph[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addNode("C")
			g = g.addEdge("A", "B")
			g = g.addEdge("C", "B")
			val inNeighborsB = g.getInNeighbors("B")
			assert(inNeighborsB == List("C", "A"))
		}
	}
}