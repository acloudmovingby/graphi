package graphi

/**
 * A directed (unweighted) implementation of MapGraph. Immutable.
 *
 * @param adjMap The adjacency map representing the graph
 * @tparam A The type of the nodes in the graph
 */
class DirectedMapGraph[A](val adjMap: Map[A, Set[A]] = Map.empty[A, Set[A]]) extends graphi.MapGraph[DirectedMapGraph[A], A] {
	// boilerplate to help inheritance work correctly, probably should be using typeclasses somehow but couldn't figure it out
	protected def returnThis: DirectedMapGraph[A] = this

	protected def constructNewThis(adjMap: Map[A, Set[A]]): DirectedMapGraph[A] = new DirectedMapGraph[A](adjMap)

	def edgeCount: Int = adjMap.values.map(_.size).sum

	/**
	 * Returns a graph with the edge added. If the edge already exists, returns `this`.
	 * Throws NoSuchElementException if either node doesn't exist.
	 */
	def addEdgeInternal(from: A, to: A): Map[A, Set[A]] = {
		// add both edges since this is an undirected graph
		val fromTo = from -> (adjMap(from) + to)
		adjMap + fromTo
	}

	def getEdges: Set[(A, A)] = {
		val edges = scala.collection.mutable.Set[(A, A)]()
		for {
			(from, neighbors) <- adjMap
			to <- neighbors
		} edges.add((from, to))
		edges.toSet
	}

	def toDot: String = toDotInternal(directed = true)
}

