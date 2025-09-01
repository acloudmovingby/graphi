package graphi
class MapBasedSimpleGraphImmutable[A](private val adjMap: Map[A, Set[A]] = Map.empty[A, Set[A]]) {
	def nodeCount: Int = adjMap.size
	def edgeCount: Int = adjMap.values.map(_.size).sum / 2

	/** Returns a graph with the node added, unless it already exists in which it returns `this` */
	def addNode(node: A): MapBasedSimpleGraphImmutable[A] = {
		if (adjMap.contains(node)) this
		else new MapBasedSimpleGraphImmutable[A](adjMap + (node -> Set()))
	}
	/**
	 * Returns a graph with the edge added. If the edge already exists, returns `this`.
	  * Throws NoSuchElementException if either node doesn't exist.
	  */
	def addEdge(from: A, to: A): MapBasedSimpleGraphImmutable[A] = {
		if (!adjMap.contains(from)) throw new NoSuchElementException(s"The node $from doesn't exist")
		else if (!adjMap.contains(to)) throw new NoSuchElementException(s"The node $to doesn't exist")
		else if (adjMap(from).contains(to)) this // edge already exists
		else {
			// add both edges since this is an undirected graph
			val fromTo = from -> (adjMap(from) + to)
			val toFrom = to -> (adjMap(to) + from)
			new MapBasedSimpleGraphImmutable[A](adjMap ++ Seq(fromTo, toFrom))
		}
	}

	/** Returns true if there is an edge between the two nodes. Throws NoSuchElementException if either node doesn't exist. */
	def hasEdge(from: A, to: A): Boolean = adjMap.get(from).exists(_.contains(to))

	/** Returns the set of neighbors of the given node. Throws NoSuchElementException if the node doesn't exist. */
	def getNeighbors(node: A): Set[A] = adjMap(node)
}
