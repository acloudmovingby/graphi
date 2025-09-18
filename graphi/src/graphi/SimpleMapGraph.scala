package graphi

/**
 * A simple (undirected, unweighted) implementation of MapGraph. Immutable.
 *
 * @param adjMap
 * @tparam A The type of the nodes in the graph
 */
class SimpleMapGraph[A](val adjMap: Map[A, Set[A]] = Map.empty[A, Set[A]]) extends MapGraph[SimpleMapGraph[A], A] {
	// boilerplate to help inheritance work correctly, probably should be using typeclasses somehow but couldn't figure it out
	override protected def returnThis: SimpleMapGraph[A] = this
	override protected def constructNewThis(adjMap: Map[A, Set[A]]): SimpleMapGraph[A] = new SimpleMapGraph[A](adjMap)

	// For undirected graphs, we count each bidirectional edge only once (hence dividing by 2)
	override def edgeCount: Int = adjMap.values.map(_.size).sum / 2

	override protected def addEdgeInternal(from: A, to: A): Map[A, Set[A]] = {
		// add both edges since this is an undirected graph
		val fromTo = from -> (adjMap(from) + to)
		val toFrom = to -> (adjMap(to) + from)
		adjMap ++ Seq(fromTo, toFrom)
	}

	override def getEdges: Set[(A, A)] = {
		val edges = scala.collection.mutable.Set[(A, A)]()
		for {
			(from, neighbors) <- adjMap
			to <- neighbors
			if from.hashCode() <= to.hashCode() // avoid duplicates in undirected graph
		} edges.add((from, to))
		edges.toSet
	}

	def toDot: String = toDotInternal(directed = false)

	// Unique to SimpleMapGraph
	override def getNeighbors(node: A): Set[A] = adjMap(node)

	def getDegree(node: A): Int = adjMap(node).size
}
