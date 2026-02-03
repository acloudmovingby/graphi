package graphi

/**
 * A base trait for immutable graph implementations using a Map-based adjacency list.
 * Nodes are of type A. Edges are unweighted.
 *
 * @tparam A The type of the nodes in the graph. Must support equality/hashing since it's the key of the internal Map.
 */
trait MapGraph[A] {

	// The concrete graph type extending this trait (allows methods to return their own type in builder pattern)
	type This <: MapGraph[A]

	val adjMap: Map[A, Set[A]]

	// protected methods to be implemented by subclasses
	protected def returnThis: This

	protected def constructNewThis(adjMap: Map[A, Set[A]]): This

	protected def addEdgeInternal(from: A, to: A): Map[A, Set[A]]

	def nodeCount: Int = adjMap.size

	def edgeCount: Int

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

	def getEdges: Set[(A, A)]

	def removeEdge(from: A, to: A): This

	// public methods with implementations

	/** Returns a graph with the node added, unless it already exists in which it returns `this` */
	def addNode(node: A): This = {
		if (adjMap.contains(node)) returnThis
		else constructNewThis(adjMap + (node -> Set()))
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

	/** Returns the set of neighbors of the given node. Throws NoSuchElementException if the node doesn't exist. */
	def getNeighbors(node: A): Set[A]

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

	def clone(nodeCloneFunction: A => A): This = {
		// create a mapping from old nodes to new nodes
		val oldToNew = adjMap.keys.map(n => n -> nodeCloneFunction(n)).toMap
		// create new adjacency map with cloned nodes
		val newAdjMap = adjMap.map { case (node, neighbors) =>
			oldToNew(node) -> neighbors.map(oldToNew)
		}
		constructNewThis(newAdjMap)
	}

	/** Set of nodes that have no edges (in or out) */
	def isolates: Set[A] = {
		val allNodes = adjMap.keys.toSet
		val hasOutEdge = adjMap.filter { case (_, neighbors) => neighbors.nonEmpty }.keySet
		val hasInEdge = adjMap.values.toSet.flatten
		allNodes -- hasOutEdge -- hasInEdge
	}

	/** Get edges, but for each edge return true if it is bidirectional and false if not. Removes redundant edges.
	 * So, for example, if graph contains edges (A,B) and (B,A), this may return either ((A,B), true) or ((B, A), true) 
	 * but not both. */
	def uniqueEdgesWithDirection: Set[((A, A), Boolean)] = {
		getEdges.foldLeft(Set[((A, A), Boolean)]()) { (acc, edge) =>
			val (from, to) = edge
			if (acc.exists { case ((f, t), _) => (f == to && t == from) }) {
				// already have the reverse edge recorded as bidirectional
				acc
			} else if (hasEdge(to, from)) {
				// add as bidirectional
				acc + (((from, to), true))
			} else {
				// add as unidirectional
				acc + (((from, to), false))
			}
		}
	}
}