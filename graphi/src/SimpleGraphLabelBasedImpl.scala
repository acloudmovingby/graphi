package graphi

import GraphTraits.SimpleGraphLabelBased

class SimpleGraphLabelBasedImpl[T] extends SimpleGraphLabelBased[T] {
	private var adjacencyList: Seq[Seq[Int]] = Seq()
	private var indexFromValue: Map[T, Int] = Map()
	private var valueFromIndex: Map[Int, T] = Map()

	def addNode(nodeValue: T): Int = {
		indexFromValue.getOrElse(nodeValue, {
			val newIndex = adjacencyList.length
			adjacencyList = adjacencyList :+ Seq()
			indexFromValue = indexFromValue + (nodeValue -> newIndex)
			valueFromIndex = valueFromIndex + (newIndex -> nodeValue)
			newIndex
		})
	}

	def addEdge(srcValue: T, destValue: T): Boolean = {
		(indexFromValue.get(srcValue), indexFromValue.get(destValue)) match {
			case (Some(srcIndex), Some(destIndex)) =>
				adjacencyList = adjacencyList.updated(srcIndex, adjacencyList(srcIndex) :+ destIndex)
				true
			case _ => false
		}
	}
}
