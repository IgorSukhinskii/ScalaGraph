package graph

import graph.AdjacencyMatrixInstances._

object Graph {
  def main(args: Array[String]){
    import GraphAlgorithms._
    val graph = FiniteGraph[AdjacencyMatrix].create(Seq(Unit, Unit, Unit, Unit, Unit, Unit, Unit),
      Seq(((0, 1), Unit),
        ((1, 0), Unit),
        ((0, 4), Unit),
        ((4, 0), Unit),
        ((0, 5), Unit),
        ((5, 0), Unit),
        ((5, 3), Unit),
        ((3, 5), Unit),
        ((3, 4), Unit),
        ((4, 3), Unit),
        ((4, 2), Unit),
        ((2, 4), Unit),
        ((2, 3), Unit),
        ((3, 2), Unit),
        ((2, 6), Unit),
        ((6, 2), Unit),
        ((6, 1), Unit),
        ((1, 6), Unit),
        ((2, 1), Unit),
        ((1, 2), Unit),
        ((1, 4), Unit),
        ((4, 1), Unit)))
//    val b = boruvka(graph)
//    showGraph(b)
//    println(sumGraph(b))
//    println()
//    val k = kruskal(graph)
//    showGraph(k)
//    println(sumGraph(k))
//    println()
//    val p = prim(graph)
//    showGraph(p)
//    println(sumGraph(p))
//    println()
    println(euler(graph))
    println()
    println(fleury(graph))
  }
}
