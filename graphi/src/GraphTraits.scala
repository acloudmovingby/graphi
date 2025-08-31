package graphi

object GraphTraits {
    trait Graph
    // Simple means undirected and unweighted but index-based
    trait SimpleGraph extends Graph {
        def addNode(): Int // TODO do I need to return index or just boolean?
        def addEdge(src: Int, dest: Int): Boolean
    }
    // Same as simple graph but API uses labels/weights for nodes not indices
    trait SimpleGraphLabelBased[A] extends Graph {
        def addNode(nodeValue: A): Int
        def addEdge(src: A, dest: A): Boolean
    }
    // Directed and unweighted but index-based
    trait DirectedGraph extends Graph {
        def addNode(): Int
        def addEdge(src: Int, dest: Int): Boolean
    }
    // Directed and unweighted but API uses labels/weights for nodes not indices
    trait DirectedGraphLabelBased[A] extends Graph {
        def addNode(nodeValue: A): Int
        def addEdge(src: A, dest: A): Boolean
    }
    // Can use these for both directed and undirected graphs, addEdge method will use default weight
    trait WeightedEdges[W] {
        def addEdge(src: Int, dest: Int, weight: W): Boolean
    }
    trait WeightedEdgesLabelBased[A, W] {
        val defaultEdgeWeight: W
        def addEdge(src: A, dest: A, weight: W): Boolean
    }
}

trait GraphBuilder { 
    def nodeValues[A]: GraphBuilder
}
