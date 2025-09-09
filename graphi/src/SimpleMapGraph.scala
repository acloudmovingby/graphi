package graphi

/**
 * A simple immutable undirected graph implementation using a map-based adjacency list.
 * Nodes are of type A. Edges are unweighted and undirected.
 * A must be a type that supports equality and hashing (e.g., Int, String, case class).
 * @param adjMap
 * @tparam A
 */
class SimpleMapGraph[A](val adjMap: Map[A, Set[A]] = Map.empty[A, Set[A]]) {
	def nodeCount: Int = adjMap.size
	def edgeCount: Int = adjMap.values.map(_.size).sum / 2

	/** Returns a graph with the node added, unless it already exists in which it returns `this` */
	def addNode(node: A): SimpleMapGraph[A] = {
		if (adjMap.contains(node)) this
		else new SimpleMapGraph[A](adjMap + (node -> Set()))
	}
	/**
	 * Returns a graph with the edge added. If the edge already exists, returns `this`.
	  * Throws NoSuchElementException if either node doesn't exist.
	  */
	def addEdge(from: A, to: A): SimpleMapGraph[A] = {
		if (!adjMap.contains(from)) throw new NoSuchElementException(s"The node $from doesn't exist")
		else if (!adjMap.contains(to)) throw new NoSuchElementException(s"The node $to doesn't exist")
		else if (adjMap(from).contains(to)) this // edge already exists
		else {
			// add both edges since this is an undirected graph
			val fromTo = from -> (adjMap(from) + to)
			val toFrom = to -> (adjMap(to) + from)
			new SimpleMapGraph[A](adjMap ++ Seq(fromTo, toFrom))
		}
	}

	/** Returns true if there is an edge between the two nodes. Throws NoSuchElementException if either node doesn't exist. */
	def hasEdge(from: A, to: A): Boolean = adjMap.get(from).exists(_.contains(to))

	/** Returns the set of neighbors of the given node. Throws NoSuchElementException if the node doesn't exist. */
	def getNeighbors(node: A): Set[A] = adjMap(node)
	
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

	def clone(nodeCloneFunction: A => A): SimpleMapGraph[A] = {
		// create a mapping from old nodes to new nodes
		val oldToNew = adjMap.keys.map(n => n -> nodeCloneFunction(n)).toMap
		// create new adjacency map with cloned nodes
		val newAdjMap = adjMap.map { case (node, neighbors) =>
			oldToNew(node) -> neighbors.map(oldToNew)
		}
		new SimpleMapGraph[A](newAdjMap)
	}
}
