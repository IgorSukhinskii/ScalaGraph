package graph

// тайпкласс, определяющий основные операции над конечным графом
// G -- конструктор типа с двумя параметрами: типом веса вершины и типом веса ребра
trait FiniteGraph[G[_, _]] {
  // VC и EC -- типы, отвечающие за количество вершин и ребер.
  // лучше объявить их в одном месте, чем искать потом по коду примитивные типы
  type VC = Int
  type EC = Int
  type V = Int
  type E = (Int, Int)

  def create[VW, EW](verts: Seq[VW], es: Seq[((V, V), EW)]): G[VW, EW]
  def createNonWeighted(verts: Seq[V], es: Seq[(V, V)]): G[Unit, Unit]

  def edge[VW, EW](g: G[VW, EW])(v0: V, v1: V): Option[EW]
  def edgeNum[VW, EW](g: G[VW, EW])(v0: V, v1: V): Option[E]
  def edgeWeight[VW, EW](g: G[VW, EW])(e: E): EW
  def edgeVertices[VW, EW](g: G[VW, EW])(e: E): (V, V)
  def edges[VW, EW](g: G[VW, EW]): Seq[E] // в мутабельных графах грани не обязательно 0..m-1
  def edgesCount[VW, EW](g: G[VW, EW]): EC

  def vertex[VW, EW](g: G[VW, EW])(e0: E, e1: E): Option[VW]
  def vertexNum[VW, EW](g: G[VW, EW])(e0: E, e1: E): Option[V]
  def vertexWeight[VW, EW](g: G[VW, EW])(v: V): Option[VW]
  def verticesNums[VW, EW](g: G[VW, EW]): Seq[V] // в мутабельных графах вершины не обязательно 0..m-1
  def vertices[VW, EW](g: G[VW, EW]): Seq[VW]
  def verticesCount[VW, EW](g: G[VW, EW]): VC

  def areIncident[VW, EW](g: G[VW, EW])(e: E, v: V, to: Boolean = true): Boolean
  def incidentEdges[VW, EW](g: G[VW, EW])(v: V, to: Boolean = true): Seq[E]

  def areAdjacent[VW, EW](g: G[VW, EW])(v0: V, v1: V, to: Boolean = true): Boolean
  def adjacentVertices[VW, EW](g: G[VW, EW])(v: V, to: Boolean = true): Seq[V]

  def setEdge[VW, EW](g: G[VW, EW])(e: E, ewo: Option[EW]): Unit

  def setVertex[VW, EW](g: G[VW, EW])(v: V, vwo: Option[VW]): Unit
}


// companion object для тайпкласса FiniteGraph
// позволяет получить доступ к инстансу этого тайпкласса
object FiniteGraph {
  def apply[G[_,_]](implicit G: FiniteGraph[G]): FiniteGraph[G] = G
}
