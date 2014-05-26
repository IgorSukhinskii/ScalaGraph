package graph

import scala.collection.mutable

// тип, представляющий собой структуру данных для хранения графа "матрица смежности"
// никаких методов работы с графом в этом типе не определяется
case class AdjacencyMatrix[VW, EW](verts: mutable.Map[Int, VW], matrix: mutable.Map[(Int, Int), EW])

// объект, который содержит в себе основные инстансы тайпклассов для AdjacencyMatrix
// в настоящий момент -- только для тайпкласса FiniteGraph
object AdjacencyMatrixInstances {
  // инстанс тайпкласса FiniteGraph для типа Adjacencyatrix -- это неявный объект,
  // который наследуется от FiniteGraph[AdjacencyMatrix], и определяет все необходимые
  // методы этого тайпкласса
  implicit object FiniteGraphAdjacencyMatrix extends FiniteGraph[AdjacencyMatrix] {

    override type V = Int
    override type E = (V, V)

    def edge[VW, EW](g: AdjacencyMatrix[VW, EW])(v0: V, v1: V): Option[EW] = g.matrix.get((v0,v1))

    def create[VW, EW](verts: Seq[VW], es: Seq[((V, V), EW)]): AdjacencyMatrix[VW, EW] = {
      val n = verts.length
      val vs: mutable.Map[V, VW] = mutable.Map.empty
      for (v <- Range(0, n)) {
        vs(v) = verts(v)
      }
      val matrix: mutable.Map[(Int, Int), EW] = mutable.Map.empty
      for (((v0, v1), ew) <- es)
        matrix((v0,v1)) = ew
      AdjacencyMatrix[VW, EW](vs, matrix)
    }
    
    def createNonWeighted(verts: Seq[V], es: Seq[(V, V)]): AdjacencyMatrix[Unit, Unit] = {
      val n = verts.length
      val vs: mutable.Map[V, Unit] = mutable.Map.empty
      for (v <- verts) {
        vs.update(v, Unit)
      }
      val matrix: mutable.Map[(Int, Int), Unit] = mutable.Map.empty
      for ((v0, v1) <- es){
        matrix.update((v0, v1), Unit)
        matrix.update((v1, v0), Unit)
      }
      AdjacencyMatrix[Unit, Unit](vs, matrix)
    }

    def edgeNum[VW, EW](g: AdjacencyMatrix[VW, EW])(v0: V, v1: V): Option[E] =
      if (g.verts.contains(v0) && g.verts.contains(v1) && g.matrix.contains((v0, v1)))
        Some((v0, v1))
      else
        None

    def edgeWeight[VW, EW](g: AdjacencyMatrix[VW, EW])(e: E): EW = g.matrix((e._1, e._2))

    def edgeVertices[VW, EW](g: AdjacencyMatrix[VW, EW])(e: E): (V, V) = (e._1, e._2)

    def edges[VW, EW](g: AdjacencyMatrix[VW, EW]): Seq[E] = // O(n^2), неэффективно в данном представлении
      for (v0 <- this.verticesNums(g); v1 <- this.verticesNums(g) if g.matrix.contains((v0, v1)))
      yield (v0, v1)

    def edgesCount[VW, EW](g: AdjacencyMatrix[VW, EW]): EC = this.edges(g).length

    def vertex[VW, EW](g: AdjacencyMatrix[VW, EW])(e0: E, e1: E): Option[VW] =
      for (v <- this.vertexNum(g)(e0, e1)) yield g.verts(v)

    def vertexNum[VW, EW](g: AdjacencyMatrix[VW, EW])(e0: E, e1: E): Option[V] =
      if (e0._2 == e1._1) Some(e0._2) else None

    def vertexWeight[VW, EW](g: AdjacencyMatrix[VW, EW])(v: V): Option[VW] =
      g.verts.get(v)

    def verticesNums[VW, EW](g: AdjacencyMatrix[VW, EW]): Seq[V] =
      g.verts.keys.toSeq

    def vertices[VW, EW](g: AdjacencyMatrix[VW, EW]): Seq[VW] =
      g.verts.values.toSeq

    def verticesCount[VW, EW](g: AdjacencyMatrix[VW, EW]): VC = g.verts.size

    def areIncident[VW, EW](g: AdjacencyMatrix[VW, EW])(e: this.E, v: this.V, to: Boolean = true): Boolean =
      if (to) e._2 == v else e._1 == v

    def incidentEdges[VW, EW](g: AdjacencyMatrix[VW, EW])(v: V, to: Boolean = true): Seq[E] = // O(n)
      this.adjacentVertices(g)(v, to).map(v0 => (v, v0))

    def areAdjacent[VW, EW](g: AdjacencyMatrix[VW, EW])(v0: V, v1: V, to: Boolean = true): Boolean =
      this.edge(g)(v0, v1).isDefined

    def adjacentVertices[VW, EW](g: AdjacencyMatrix[VW, EW])(v: V, to: Boolean = true): Seq[V] =
      this.verticesNums(g).filter(v1 => this.areAdjacent(g)(v, v1, to))

    def setEdge[VW, EW](g: AdjacencyMatrix[VW, EW])(e: E, ewo: Option[EW]): Unit =
      ewo match {
        case Some(ew) => g.matrix.update(e, ew)
        case None     => g.matrix.remove(e)
      }

    def setVertex[VW, EW](g: AdjacencyMatrix[VW, EW])(v: V, vwo: Option[VW]): Unit =
      vwo match {
        case Some(vw) => g.verts(v) = vw
        case None     => g.verts.remove(v)
      }
  }
}
