package graphi

/**
 * A directed (unweighted) implementation of MapGraph. Immutable.
 *
 * @param adjMap The adjacency map representing the graph
 * @tparam A The type of the nodes in the graph
 */
class DirectedMapGraph[A](val adjMap: Map[A, Set[A]] = Map.empty[A, Set[A]]) extends graphi.MapGraph[DirectedMapGraph[A], A] {
	// boilerplate to help inheritance + immutability work correctly, probably should be using typeclasses somehow but couldn't figure it out
	override protected def returnThis: DirectedMapGraph[A] = this

	override protected def constructNewThis(adjMap: Map[A, Set[A]]): DirectedMapGraph[A] = new DirectedMapGraph[A](adjMap)

	override def edgeCount: Int = adjMap.values.map(_.size).sum

	/**
	 * Returns a graph with the edge added. If the edge already exists, returns `this`.
	 * Throws NoSuchElementException if either node doesn't exist.
	 */
	override protected def addEdgeInternal(from: A, to: A): Map[A, Set[A]] = {
		// add both edges since this is an undirected graph
		val fromTo = from -> (adjMap(from) + to)
		adjMap + fromTo
	}

	override def getEdges: Set[(A, A)] = {
		val edges = scala.collection.mutable.Set[(A, A)]()
		for {
			(from, neighbors) <- adjMap
			to <- neighbors
		} edges.add((from, to))
		edges.toSet
	}

	override def toDot: String = toDotInternal(directed = true)

	override def getNeighbors(node: A): Set[A] = getSuccessors(node)

	// Unique to DirectedMapGraph

    /** Returns the set of successor nodes (i.e., nodes directly reachable from the given node).
	  * Throws NoSuchElementException if the node doesn't exist.
	  */
	def getSuccessors(node: A): Set[A] = {
		adjMap.getOrElse(node, throw new NoSuchElementException(s"The node $node doesn't exist"))
	}

	/** Returns the set of predecessor nodes (i.e., nodes that can reach the given node directly).
	 * Currently O(E) where E = number of edges, since we have to scan the whole adjacency map.
	  * Throws NoSuchElementException if the node doesn't exist.
	  */
	def getPredecessors(node: A): Set[A] = {
		if (!adjMap.contains(node)) throw new NoSuchElementException(s"The node $node doesn't exist")
		adjMap.filter { case (_, neighbors) => neighbors.contains(node) }.keys.toSet
	}

	def getOutDegree(node: A): Int = {
		adjMap.getOrElse(node, throw new NoSuchElementException(s"The node $node doesn't exist")).size
	}

	// TODO optimize getInDegree to avoid O(E) complexity
	// Currently O(E) where E = number of edges, since we have to scan the whole adjacency map.
	def getInDegree(node: A): Int = {
		if (!adjMap.contains(node)) throw new NoSuchElementException(s"The node $node doesn't exist")
		adjMap.count { case (_, neighbors) => neighbors.contains(node) }
	}
}

