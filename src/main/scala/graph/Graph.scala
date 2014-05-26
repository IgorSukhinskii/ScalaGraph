package graph

import graph.AdjacencyMatrixInstances._

object Graph {
  def main(args: Array[String]){
    import GraphAlgorithms._
    val graph0 = FiniteGraph[AdjacencyMatrix].createNonWeighted(Seq(1, 2, 3, 4, 5, 6, 7, 8), Seq(
      (1, 2),
      (2, 3),
      (3, 4),
      (4, 5),
      (5, 6),
      (6, 7),
      (7, 3),
      (8, 5)
    ))
    val graph1 = FiniteGraph[AdjacencyMatrix].createNonWeighted(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Seq(
      (1, 2),
      (2, 3),
      (3, 5),
      (4, 5),
      (5, 6),
      (6, 4),
      (7, 6),
      (8, 7),
      (8, 5),
      (8, 9),
      (9, 10),
      (3, 10)
    ))
    val graph2 = FiniteGraph[AdjacencyMatrix].createNonWeighted(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Seq(
      (1, 2),
      (2, 3),
      (3, 4),
      (4, 5),
      (5, 4),
      (6, 1),
      (7, 6),
      (8, 7),
      (8, 10),
      (10, 9),
      (9, 6),
      (5, 1)
    ))
    val graph3 = FiniteGraph[AdjacencyMatrix].createNonWeighted(Seq(1, 2, 3, 4, 5, 6, 7), Seq(
      (1, 2),
      (2, 3),
      (3, 4),
      (4, 5),
      (5, 4),
      (6, 1),
      (7, 6),
      (4, 7),
      (2, 6),
      (3, 6),
      (4, 6),
      (5, 6)
    ))
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
    showGraph(blossomAlgorithm(graph0))
    println()
    showGraph(blossomAlgorithm(graph1))
    println()
    showGraph(blossomAlgorithm(graph2))
    println()
    showGraph(blossomAlgorithm(graph3))
  }
}
