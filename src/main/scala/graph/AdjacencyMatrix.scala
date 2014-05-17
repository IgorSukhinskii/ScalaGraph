package graph

// тип, представляющий собой структуру данных для хранения графа "матрица смежности"
// никаких методов работы с графом в этом типе не определяется
case class AdjacencyMatrix[VW, EW](verts: Vector[VW], matrix: Array[Array[Option[EW]]])

// объект, который содержит в себе основные инстансы тайпклассов для AdjacencyMatrix
// в настоящий момент -- только для тайпкласса FiniteGraph
object AdjacencyMatrixInstances {
  // инстанс тайпкласса FiniteGraph для типа Adjacencyatrix -- это неявный объект,
  // который наследуется от FiniteGraph[AdjacencyMatrix], и определяет все необходимые
  // методы этого тайпкласса
  implicit object FiniteGraphAdjacencyMatrix extends FiniteGraph[AdjacencyMatrix] {

    override type V = Int
    override type E = (V, V)

    def edge[VW, EW](g: AdjacencyMatrix[VW, EW])(v0: V, v1: V): Option[EW] = g.matrix(v0)(v1)

    def create[VW, EW](verts: Seq[VW], es: Seq[((V, V), EW)]): AdjacencyMatrix[VW, EW] = {
      val n = verts.length
      val m = Array.fill[Option[EW]](n, n)(None)
      for (((v0, v1), ew) <- es)
        m(v0)(v1) = Some(ew)
      AdjacencyMatrix[VW, EW](verts.toVector, m)
    }

    def edgeNum[VW, EW](g: AdjacencyMatrix[VW, EW])(v0: V, v1: V): Option[E] =
      if ((v0 >= 0 && v0 < g.verts.length) && (v1 >= 0 && v1 < g.verts.length) && g.matrix(v0)(v1).isDefined)
        Some((v0, v1))
      else
        None

    def edgeWeight[VW, EW](g: AdjacencyMatrix[VW, EW])(e: E): EW = g.matrix(e._1)(e._2).get

    def edgeVertices[VW, EW](g: AdjacencyMatrix[VW, EW])(e: E): (V, V) = (e._1, e._2)

    def edges[VW, EW](g: AdjacencyMatrix[VW, EW]): Seq[E] = // O(n^2), неэффективно в данном представлении
      for (v0 <- this.verticesNums(g); v1 <- this.verticesNums(g) if g.matrix(v0)(v1).isDefined)
      yield (v0, v1)

    def edgesCount[VW, EW](g: AdjacencyMatrix[VW, EW]): EC = this.edges(g).length

    def vertex[VW, EW](g: AdjacencyMatrix[VW, EW])(e0: E, e1: E): Option[VW] =
      for (v <- this.vertexNum(g)(e0, e1)) yield g.verts(v)

    def vertexNum[VW, EW](g: AdjacencyMatrix[VW, EW])(e0: E, e1: E): Option[V] =
      if (e0._2 == e1._1) Some(e0._2) else None

    def vertexWeight[VW, EW](g: AdjacencyMatrix[VW, EW])(v: V): Option[VW] =
      if (v >= 0 && v < g.verts.length) Some(g.verts(v)) else None

    def verticesNums[VW, EW](g: AdjacencyMatrix[VW, EW]): Seq[V] =
      Range(0, g.verts.length) // должно быть O(1), если Скалу писали не полные кретины

    def vertices[VW, EW](g: AdjacencyMatrix[VW, EW]): Seq[VW] =
      g.verts

    def verticesCount[VW, EW](g: AdjacencyMatrix[VW, EW]): VC = g.verts.length

    def areIncident[VW, EW](g: AdjacencyMatrix[VW, EW])(e: this.E, v: this.V, to: Boolean = true): Boolean =
      if (to) e._2 == v else e._1 == v

    def incidentEdges[VW, EW](g: AdjacencyMatrix[VW, EW])(v: V, to: Boolean = true): Seq[E] = // O(n)
      this.adjacentVertices(g)(v, to).map(v0 => (v, v0))

    def areAdjacent[VW, EW](g: AdjacencyMatrix[VW, EW])(v0: V, v1: V, to: Boolean = true): Boolean =
      this.edge(g)(v0, v1).isDefined

    def adjacentVertices[VW, EW](g: AdjacencyMatrix[VW, EW])(v: V, to: Boolean = true): Seq[V] =
      this.verticesNums(g).filter(v1 => this.areAdjacent(g)(v, v1, to))

    def setEdge[VW, EW](g: AdjacencyMatrix[VW, EW])(e: E, ew: Option[EW]): Unit = g.matrix(e._1)(e._2) = ew
  }
}
