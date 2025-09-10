package graphi
import utest.*
import graphi.SimpleMapGraph
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
		
		test("clone") {
			// create a graph, clone it, and ensure the clone is identical but independent
			var g1 = new SimpleMapGraph[String]()
			for (node <- Seq("A", "B", "C")) {
				g1 = g1.addNode(node)
			}
			g1 = g1.addEdge("A", "B")
			g1 = g1.addEdge("B", "C")
			// clone
			val g2 = g1.clone(s => s) // identity function for cloning
			// ensure identical
			assert(g1.nodeCount == g2.nodeCount)
			assert(g1.edgeCount == g2.edgeCount)
			for (node <- Seq("A", "B", "C")) {
				assert(g1.getNeighbors(node) == g2.getNeighbors(node))
			}
			// modify original and ensure clone is unaffected
			g1 = g1.addNode("D")
			g1 = g1.addEdge("C", "D")
			assert(g1.nodeCount == 4)
			assert(g1.edgeCount == 3)
			assert(g2.nodeCount == 3)
			assert(g2.edgeCount == 2)
			assert(!g2.hasEdge("C", "D"))
		}
		
		test("cloneWithMutableData") {
			// create a graph with mutable data (e.g., ListBuffer), clone it, and ensure the clone is independent
			import scala.collection.mutable.ListBuffer
			var g1 = new SimpleMapGraph[ListBuffer[Int]]()
			val nodeA = ListBuffer(1)
			val nodeB = ListBuffer(2)
			val nodeC = ListBuffer(3)
			g1 = g1.addNode(nodeA)
			g1 = g1.addNode(nodeB)
			g1 = g1.addNode(nodeC)
			g1 = g1.addEdge(nodeA, nodeB)
			g1 = g1.addEdge(nodeB, nodeC)
			// clone
			val g2 = g1.clone(lb => lb.clone()) // deep clone of ListBuffer
			// modify original graph's node data
			nodeA += 10
			nodeB += 20
			// ensure original graph's node data is modified
			val originalNodeA = g1.adjMap.keys.find(_.contains(1)).get
			val originalNodeB = g1.adjMap.keys.find(_.contains(2)).get
			assert(originalNodeA == ListBuffer(1, 10))
			assert(originalNodeB == ListBuffer(2, 20))
			// ensure clone's node data is unaffected
			val clonedNodeA = g2.adjMap.keys.find(_.contains(1)).get
			val clonedNodeB = g2.adjMap.keys.find(_.contains(2)).get
			assert(clonedNodeA == ListBuffer(1))
			assert(clonedNodeB == ListBuffer(2))
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
			val expectedLines = Set(
				"graph G {",
				""""A" -- "B";""",
				""""B" -- "C";""",
				""""D";""",
				"}"
			)
			val dotLines = dot.split("\n").map(_.trim).toSet
			assert(dotLines == expectedLines)
		}
	}
}