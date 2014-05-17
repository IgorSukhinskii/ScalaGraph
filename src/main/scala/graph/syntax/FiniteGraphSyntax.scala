package graph.syntax

import scalaz.syntax.Ops
import graph.FiniteGraph

// этот класс существует для реализации идиомы Enrich My Library
// объект этого класса инкапсулирует в себе объект "улучшаемого" типа
// и добавляет к нему некоторые методы
// благодаря наличию неявного преобразования из "улучшаемого" типа в "улучшенный"
//
// FiniteGraphOps возможно выполнять операции наподобие g.vertices,
// где g: SomeType, SomeType не имеет метода vertices, но существует неявное преобразование
// из SomeType в FiniteGraphOps
final class FiniteGraphOps[G[_, _], VW, EW] private[syntax]
(val self: G[VW, EW])(implicit val G: FiniteGraph[G]) extends Ops[G[VW, EW]] {
  // все эти операции уже определены в тайпклассе FiniteGraph,
  // здесь только меняется манера вызова ( g.vertices вместо FiniteGraph[G].vertices(g) )
  def edge = G.edge(self) _
  def edgeNum = G.edgeNum(self) _
  def edgeWeight = G.edgeWeight(self) _
  def edgeVertices = G.edgeVertices(self) _
  def edges = G.edges(self)
  def edgesCount = G.edgesCount(self)

  def vertex = G.vertex(self) _
  def vertexNum = G.vertexNum(self) _
  def vertexWeight = G.vertexWeight(self) _
  def verticesNums = G.verticesNums(self)
  def vertices = G.vertices(self)
  def verticesCount = G.verticesCount(self)

  def areIncident = G.areIncident(self) _
  def incidentEdges = G.incidentEdges(self) _

  def areAdjacent = G.areAdjacent(self) _
  def adjacentVertices = G.adjacentVertices(self) _

  def setEdge = G.setEdge(self) _
}

// трейт определяет неявное преобразование из любого конечного графа в FiniteGraphOps,
// таким образом "добавляя" новые методы
trait ToFiniteGraphOps {
  implicit def ToFiniteGraphOps[G[_,_]: FiniteGraph, VW, EW](v: G[VW, EW]) = new FiniteGraphOps[G, VW, EW](v)
}

trait FiniteGraphSyntax[G[_,_]] {
  implicit def ToFiniteGraphOps[VW, EW](v: G[VW, EW]): FiniteGraphOps[G, VW, EW] =
    new FiniteGraphOps[G, VW, EW](v)(FiniteGraphSyntax.this.G)

  def G: FiniteGraph[G]
}
