package graphi

import GraphTraits.DirectedGraphLabelBased

class DirectedGraphLabelBasedImpl[A] extends DirectedGraphLabelBased[A] {
	private var adjacencyList: Seq[Seq[Int]] = Seq()
	private var indexFromValue: Map[A, Int] = Map()
	private var valueFromIndex: Map[Int, A] = Map()

	def addNode(nodeValue: A): Int = {
		indexFromValue.getOrElse(nodeValue, {
			val newIndex = adjacencyList.length
			adjacencyList = adjacencyList :+ Seq()
			indexFromValue = indexFromValue + (nodeValue -> newIndex)
			valueFromIndex = valueFromIndex + (newIndex -> nodeValue)
			newIndex
		})
	}

	def addEdge(srcValue: A, destValue: A): Boolean = {
		(indexFromValue.get(srcValue), indexFromValue.get(destValue)) match {
			case (Some(srcIndex), Some(destIndex)) =>
				if (adjacencyList(srcIndex).contains(destIndex)) {
					false // Edge already exists
				} else {
					adjacencyList = adjacencyList.updated(srcIndex, adjacencyList(srcIndex) :+ destIndex)
					true
				}
			case _ => false
		}
	}
}
