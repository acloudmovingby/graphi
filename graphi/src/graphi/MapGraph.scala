package graphi

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * A base trait for immutable graph implementations using a Map-based adjacency list.
 * Nodes are of type A. Edges are unweighted.
 *
 * @tparam A The type of the nodes in the graph. Must support equality/hashing since it's the key of the internal Map.
 */
trait MapGraph[A] {

	// The concrete graph type extending this trait (allows methods to return their own type in builder pattern)
	type This <: MapGraph[A]

	val adjMap: Map[A, List[A]]

	// protected methods to be implemented by subclasses
	protected def returnThis: This

	protected def constructNewThis(adjMap: Map[A, List[A]]): This

	protected def addEdgeInternal(from: A, to: A): Map[A, List[A]]

	def nodeCount: Int = adjMap.size

	def edgeCount: Int

	lazy val nodes: Seq[A] = adjMap.keys.toSeq

	// helper methods for subclasses
	protected def toDotInternal(directed: Boolean): String = {
		val edgeSymbol = if (directed) "->" else "--"
		val graphType = if (directed) "digraph" else "graph"
		val s = Seq(1, 2)
		val edges = getEdges
		val _isolates = this.isolates
		val edgeStrings = edges.map { case (f, t) => s"""  "${f.toString}" $edgeSymbol "${t.toString}";""" }
		val isolateStrings = _isolates.map(node => s"""  "${node.toString}";""")
		val allStrings = edgeStrings ++ isolateStrings
		val concatenated = if (allStrings.nonEmpty) allStrings.mkString("\n", "\n", "\n") else ""
		s"$graphType G {$concatenated}"
	}

	// unimplemented public methods
	def toDot: String

	def getEdges: Seq[(A, A)]

	def removeEdge(from: A, to: A): This

	// public methods with implementations

	def empty: This = constructNewThis(Map.empty)

	/** Returns a graph with the node added, unless it already exists in which it returns `this` */
	def addNode(node: A): This = {
		if (adjMap.contains(node)) returnThis
		else constructNewThis(adjMap + (node -> List()))
	}

	/**
	 * Returns a graph with the edge added. If the edge already exists, returns `this`.
	 * Throws NoSuchElementException if either node doesn't exist.
	 */
	def addEdge(from: A, to: A): This = {
		if (!adjMap.contains(from)) throw new NoSuchElementException(s"The node $from doesn't exist")
		else if (!adjMap.contains(to)) throw new NoSuchElementException(s"The node $to doesn't exist")
		else if (adjMap(from).contains(to)) returnThis // edge already exists
		else constructNewThis(addEdgeInternal(from, to))
	}

	/** Returns true if there is an edge between the two nodes. Throws NoSuchElementException if either node doesn't exist. */
	def hasEdge(from: A, to: A): Boolean = adjMap.get(from).exists(_.contains(to))

	/** Returns the (out) neighbors of the given node. Throws NoSuchElementException if the node doesn't exist. */
	def getNeighbors(node: A): List[A]

	private val inNeighborsCache: mutable.Map[A, List[A]] = {
		val cache = mutable.Map[A, List[A]]()
		for {
			(node, neighbors) <- adjMap
			neighbor <- neighbors
		} {
			cache.updateWith(neighbor) {
				case Some(existing) => Some(node :: existing)
				case None => Some(List(node))
			}
		}
		cache
	}

	def getInNeighbors(node: A): List[A] = {
		if (!adjMap.contains(node)) throw new NoSuchElementException(s"The node $node doesn't exist")
		inNeighborsCache.getOrElse(node, List())
	}

	def djikstra(start: A, end: A): Option[(List[A], Int)] = {
		if (!adjMap.contains(start)) throw new NoSuchElementException(s"The node $start doesn't exist")
		else if (!adjMap.contains(end)) throw new NoSuchElementException(s"The node $end doesn't exist")
		else {
			// Djikstra's algorithm
			import scala.collection.mutable
			val distances = mutable.Map[A, Int](start -> 0)
			val previous = mutable.Map[A, A]()
			val pq = mutable.PriorityQueue[(A, Int)]()(using Ordering.by(-_._2))
			pq.enqueue((start, 0))

			while (pq.nonEmpty) {
				val (currentNode, currentDist) = pq.dequeue()
				if (currentNode == end) {
					// build path
					var path = List[A](end)
					var step = end
					while (previous.contains(step)) {
						step = previous(step)
						path = step :: path
					}
					return Some((path, currentDist))
				}
				for (neighbor <- adjMap(currentNode)) {
					val newDist = currentDist + 1 // all edges have weight 1
					if (newDist < distances.getOrElse(neighbor, Int.MaxValue)) {
						distances(neighbor) = newDist
						previous(neighbor) = currentNode
						pq.enqueue((neighbor, newDist))
					}
				}
			}
			None // no path found
		}
	}

	/** Set of nodes that have no edges (in or out) */
	def isolates: Set[A] = {
		val allNodes = adjMap.keys.toSet
		val hasOutEdge = adjMap.filter { case (_, neighbors) => neighbors.nonEmpty }.keySet
		val hasInEdge = adjMap.values.toSeq.flatten
		allNodes -- hasOutEdge -- hasInEdge
	}

	/** Get edges, but for each edge return true if it is bidirectional and false if not. Removes redundant edges.
	 * So, for example, if graph contains edges (A,B) and (B,A), this may return either ((A,B), true) or ((B, A), true) 
	 * but not both. */
	def uniqueEdgesWithDirection: Seq[((A, A), Boolean)] = {
		val result = mutable.Set[((A, A), Boolean)]()
		val processedPairs = mutable.Set[(A, A)]() // Stores canonical (node1, node2) where node1.hashCode <= node2.hashCode

		for {
			(from, neighbors) <- adjMap
			to <- neighbors
		} {
			// Create a canonical pair to avoid processing (A,B) and then (B,A) separately
			val (node1, node2) = if (from.hashCode() <= to.hashCode()) (from, to) else (to, from)

			if (!processedPairs.contains((node1, node2))) {
				processedPairs.add((node1, node2))

				val isBidirectional = hasEdge(to, from)
				result.addOne(((from, to), isBidirectional))
			}
		}
		result.toSeq
	}

	/**
	 * Depth first search
	 * */
	def depthFirstSearch(startNode: A): List[A] = {
		val visited: mutable.Set[A] = mutable.Set.empty

		@tailrec
		def loop(stack: List[A], result: List[A]): List[A] = stack match {
			case Nil =>
				result
			case head :: tail if visited.contains(head) =>
				result
			case head :: tail =>
				visited += head
				val unvisitedNeighbors = this.getNeighbors(head).filterNot(visited.contains)
				val newStack = unvisitedNeighbors ++ tail
				loop(newStack, head :: result)
		}

		loop(startNode :: Nil, Nil).reverse
	}

	/**
	 * How many connected components are there? (Note: not *strongly* connected components)
	 */
	def numComponents(): Int = {
		components().size
//		val visited: mutable.Set[A] = mutable.Set.empty
//		val localVisited: mutable.Set[A] = mutable.Set.empty
//
//		var componentCount = 0
//
//		// as we explore the DFS tree, we'll start by assuming it's an unconnected component but if we encounter an already
//		// visited node, then we flip this flag to false to not double-count
//		var newComponent = true
//
//		/**
//		 * Similar to depthFirstSearch but we don't need to actually return the List, we just mark nodes as visited.
//		 * We'll just call this repeatedly until all nodes are visited.
//		 */
//		@tailrec
//		def dfs(stack: List[A]): Unit = stack match {
//			case Nil => ()
//			case head :: tail if localVisited.contains(head) => ()
//			case head :: tail =>
//				localVisited += head
//				val unvisitedNeighbors = this.getNeighbors(head).toList.filter { n =>
//					if (visited.contains(n)) {
//						newComponent = false
//						false
//					} else true
//				}
//				val newStack = unvisitedNeighbors ++ tail
//				dfs(newStack)
//		}
//
//		for (node <- nodes) {
//			if (!visited.contains(node)) {
//				newComponent = true // assume it might be a new component
//				dfs(node :: Nil)
//				visited.addAll(localVisited)
//				localVisited.clear()
//				if (newComponent) componentCount += 1
//				println(s"dragon - node=$node localVisited=${localVisited}, visited=${visited}, componentCount=$componentCount")
//			}
//		}
//
//		componentCount
	}

	/** Connected components */
	def components(): List[List[A]] = {
		val nodesToComponent = mutable.Map.empty[A, Int]
		val componentConnections = mutable.Map.empty[Int, mutable.Set[Int]]
		val visited = mutable.Set.empty[A]
		var currentComponent = 0

		@tailrec
		def dfs(stack: List[A], compIdx: Int): Unit = stack match {
			case Nil => ()
			case head :: tail if visited.contains(head) =>
				// If already assigned, record connection
				nodesToComponent.get(head).foreach { otherComp =>
					if (otherComp != compIdx) {
						componentConnections.getOrElseUpdate(compIdx, mutable.Set()).add(otherComp)
						()
					}
				}
				dfs(tail, compIdx)
			case head :: tail =>
				visited += head
				nodesToComponent(head) = compIdx
				dfs(getNeighbors(head) ++ tail, compIdx)
		}

		// Initial DFS to assign components and record connections
		for (node <- nodes) {
			if (!visited.contains(node)) {
				dfs(List(node), currentComponent)
				currentComponent += 1
			}
		}

		// Union-find to merge connected components
		val parent = (0 until currentComponent).toArray
		def find(x: Int): Int = if (parent(x) == x) x else { parent(x) = find(parent(x)); parent(x) }
		def union(x: Int, y: Int): Unit = parent(find(x)) = find(y)

		for ((comp, conns) <- componentConnections; conn <- conns) {
			union(comp, conn)
		}

		// Group nodes by their final component parent
		val groups = mutable.Map.empty[Int, mutable.ListBuffer[A]]
		for ((node, comp) <- nodesToComponent) {
			val root = find(comp)
			groups.getOrElseUpdate(root, mutable.ListBuffer()) += node
		}

		groups.values.map(_.toList).toList
	}
}