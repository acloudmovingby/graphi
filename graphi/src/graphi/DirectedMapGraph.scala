package graphi

/**
 * A directed (unweighted) implementation of MapGraph. Immutable.
 *
 * @param adjMap The adjacency map representing the graph
 * @tparam A The type of the nodes in the graph
 */
class DirectedMapGraph[A](val adjMap: Map[A, List[A]] = Map.empty[A, List[A]]) extends MapGraph[A] {
	type This = DirectedMapGraph[A]

	// boilerplate to help inheritance + immutability work correctly, probably should be using typeclasses somehow but couldn't figure it out
	override protected def returnThis: DirectedMapGraph[A] = this

	override protected def constructNewThis(adjMap: Map[A, List[A]]): DirectedMapGraph[A] = new DirectedMapGraph[A](adjMap)

	override def edgeCount: Int = adjMap.values.map(_.size).sum

	/**
	 * Returns a graph with the edge added. If the edge already exists, returns `this`.
	 * Throws NoSuchElementException if either node doesn't exist.
	 */
	override protected def addEdgeInternal(from: A, to: A): Map[A, List[A]] = {
		adjMap.updatedWith(from) {
			case Some(neighbors) =>
				if (neighbors.contains(to)) Some(neighbors) // edge already exists, return unchanged
				else Some(to :: neighbors) // add new edge
			case None => throw new NoSuchElementException(s"The node $from doesn't exist")
		}
	}

	private lazy val _edges = {
		for {
			(from, neighbors) <- adjMap.toList
			to <- neighbors
		} yield (from, to)
	}

	override def getEdges: List[(A, A)] = _edges

	override def toDot: String = toDotInternal(directed = true)

	override def getNeighbors(node: A): List[A] = getSuccessors(node)

	// Unique to DirectedMapGraph

	/** Returns the set of successor nodes (i.e., nodes directly reachable from the given node).
	 * Throws NoSuchElementException if the node doesn't exist.
	 */
	def getSuccessors(node: A): List[A] = {
		adjMap.getOrElse(node, throw new NoSuchElementException(s"The node $node doesn't exist"))
	}

	/** Returns the set of predecessor nodes (i.e., nodes that can reach the given node directly).
	 * Throws NoSuchElementException if the node doesn't exist.
	 */
	def getPredecessors(node: A): List[A] = {
		super.getInNeighbors(node)
	}

	/**
	 * Removes the edge from 'from' to 'to'.
	 * Only removes the edge in the directed direction.
	 * Throws NoSuchElementException if either node doesn't exist.
	 * Returns a new graph with the edge removed.
	 */
	override def removeEdge(from: A, to: A): DirectedMapGraph[A] = {
		if (!adjMap.contains(from)) throw new NoSuchElementException(s"The node $from doesn't exist")
		if (!adjMap.contains(to)) throw new NoSuchElementException(s"The node $to doesn't exist")
		val updatedAdjMap = adjMap.updatedWith(from) {
			case Some(neighbors) => Some(neighbors.filterNot(_ == to))
			case None => throw new NoSuchElementException(s"The node $from doesn't exist")
		}
		// Note: we don't update the "to" node's neighbors since this is a directed graph
		constructNewThis(updatedAdjMap)
	}

	override def toString: String = s"DirectedGraph($adjMap)"
}
