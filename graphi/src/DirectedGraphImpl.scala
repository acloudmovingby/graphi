package graphi

import GraphTraits.DirectedGraph

class DirectedGraphImpl extends DirectedGraph {
	private var adjacencyList: Seq[Seq[Int]] = Seq()

	def addNode(): Int = {
		val index = adjacencyList.length
		adjacencyList = adjacencyList :+ Seq()
		index
	}

	def addEdge(src: Int, dest: Int): Boolean = {
		if (src < 0 || src >= adjacencyList.length || dest < 0 || dest >= adjacencyList.length) {
			throw new IndexOutOfBoundsException("Source or destination index is out of bounds")
		}
		if (adjacencyList(src).contains(dest)) {
			false // Edge already exists
		} else {
			adjacencyList = adjacencyList.updated(src, adjacencyList(src) :+ dest)
			true
		}
	}
}
