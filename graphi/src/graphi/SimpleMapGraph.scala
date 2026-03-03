package graphi

/**
 * A simple (undirected, unweighted) implementation of MapGraph. Immutable.
 *
 * @param adjMap
 * @tparam A The type of the nodes in the graph
 */
class SimpleMapGraph[A](val adjMap: Map[A, List[A]] = Map.empty[A, List[A]]) extends MapGraph[A] {

	type This = SimpleMapGraph[A]

	// boilerplate to help inheritance work correctly, probably should be using typeclasses somehow but couldn't figure it out
	override protected def returnThis: SimpleMapGraph[A] = this

	override protected def constructNewThis(adjMap: Map[A, List[A]]): SimpleMapGraph[A] = new SimpleMapGraph[A](adjMap)

	// For undirected graphs, we count each bidirectional edge only once (hence dividing by 2)
	override def edgeCount: Int = adjMap.values.map(_.size).sum / 2

	override protected def addEdgeInternal(from: A, to: A): Map[A, List[A]] = {
		adjMap.updatedWith(from) {
			case Some(neighbors) => Some(to :: neighbors)
			case None => throw new NoSuchElementException(s"The node $from doesn't exist")
		}.updatedWith(to) {
			case Some(neighbors) => Some(from :: neighbors)
			case None => throw new NoSuchElementException(s"The node $to doesn't exist")
		}
	}

	override def getEdges: Seq[(A, A)] = {
		val edges = scala.collection.mutable.Set[(A, A)]()
		for {
			(from, neighbors) <- adjMap
			to <- neighbors
			if from.hashCode() <= to.hashCode() // avoid duplicates in undirected graph
		} edges.add((from, to))
		edges.toSeq
	}

	def toDot: String = toDotInternal(directed = false)

	// SimpleMapGraph has "neighbors", a mutual relationship, unlike DirectedMapGraph which has "successors" and "predecessors".
	override def getNeighbors(node: A): List[A] = adjMap(node)

	/**
	 * Removes the edge between 'from' and 'to' in both directions.
	 * Throws NoSuchElementException if either node doesn't exist.
	 * Returns a new graph with the edge removed.
	 */
	override def removeEdge(from: A, to: A): SimpleMapGraph[A] = {
		if (!adjMap.contains(from)) throw new NoSuchElementException(s"The node $from doesn't exist")
		if (!adjMap.contains(to)) throw new NoSuchElementException(s"The node $to doesn't exist")
		val updatedAdjMap = adjMap.updatedWith(from) {
			case Some(neighbors) => Some(neighbors.filterNot(_ == to))
			case None => throw new NoSuchElementException(s"The node $from doesn't exist")
		}.updatedWith(to) {
			case Some(neighbors) => Some(neighbors.filterNot(_ == from))
			case None => throw new NoSuchElementException(s"The node $to doesn't exist")
		}
		constructNewThis(updatedAdjMap)
	}

	override def toString: String = s"SimpleGraph($adjMap)"
}
