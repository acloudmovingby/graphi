package graphi
import utest.*
import graphi.MapBasedSimpleGraphImmutable
object MapBasedSimpleTests extends TestSuite {
	def tests = Tests {
		test("EmptyGraph") {
			val g = new MapBasedSimpleGraphImmutable[String]()
			assert(g.nodeCount == 0)
			assert(g.edgeCount == 0)
		}
		test("OneNodeGraph") {
			var g = new MapBasedSimpleGraphImmutable[String]()
			g = g.addNode("A")
			assert(g.nodeCount == 1)
			assert(g.edgeCount == 0)
		}
		test("TwoNodeGraph") {
			// add two nodes and add an edge between them
			var g = new MapBasedSimpleGraphImmutable[String]()
			g = g.addNode("A")
			g = g.addNode("B")
			g = g.addEdge("A", "B")
			assert(g.nodeCount == 2)
			assert(g.edgeCount == 1)
			assert(g.hasEdge("A", "B"))
			assert(g.hasEdge("B", "A"))
			assert(!g.hasEdge("A", "C"))
		}
		test("AddRedundantNodes") {
			// add two nodes with label "A" and ensure node count is still 1
			var g = new MapBasedSimpleGraphImmutable[String]()
			g = g.addNode("A")
			g = g.addNode("A")
			assert(g.nodeCount == 1)
			assert(g.edgeCount == 0)
			assert(!g.hasEdge("A", "A"))
		}
		test("AddRedundantEdges") {
			// add two nodes and add the same edge twice, ensure edge count is still 1
			var g = new MapBasedSimpleGraphImmutable[String]()
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
			var g = new MapBasedSimpleGraphImmutable[String]()
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
			var g = new MapBasedSimpleGraphImmutable[String]()
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
			assert(neighborsA == Set("B", "C"))
			assert(neighborsB == Set("A"))
			assert(neighborsC == Set("A"))
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
			var g = new MapBasedSimpleGraphImmutable[String]()
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
			var g = new MapBasedSimpleGraphImmutable[String]()
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
	}
}