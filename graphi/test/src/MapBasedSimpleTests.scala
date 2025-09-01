package graphi
import utest.*
import graphi.MapBasedSimpleGraphImmutable
object MapBasedSimpleTests extends TestSuite {
	def tests = Tests {
		test("EmptyGraph") {
			val g = new MapBasedSimpleGraphImmutable()
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
	}
}