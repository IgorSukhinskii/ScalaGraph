package graph

import scala.collection.mutable

import scalaz.{Monoid, Order}
import scalaz.syntax.order._
import scalaz.std.AllInstances._

import graph.disjointSets.DisjointSets
import graph.syntax.all._

sealed abstract class Eulerian
case object Cycle extends Eulerian
case object Chain extends Eulerian
case object NonEulerian extends Eulerian

object GraphAlgorithms extends BlossomAlgorithm{
  def isEulerian[G[_,_]: FiniteGraph, VW, EW](g: G[VW, EW]): Eulerian = {
    g.verticesNums.count(v => g.adjacentVertices(v, true).length % 2 === 1) match {
      case 0 => Cycle
      case 2 => Chain
      case _ => NonEulerian
    }
  }

  def euler[G[_,_]: FiniteGraph, VW, EW](g: G[VW, EW]): Option[Seq[FiniteGraph[G]#V]] = {
    val result: mutable.Stack[FiniteGraph[G]#V] = new mutable.Stack[FiniteGraph[G]#V]()
    val st: mutable.Stack[FiniteGraph[G]#V] = new mutable.Stack[FiniteGraph[G]#V]()
    isEulerian(g) match {
      case Cycle => st.push(g.verticesNums.head)
      case Chain => st.push(g.verticesNums.filter(v => g.adjacentVertices(v, true).length % 2 === 1).head)
      case NonEulerian => return None
    }

    val g1 = FiniteGraph[G].create(g.vertices, g.edges.map(e => ((e._1, e._2), Unit)))

    while (st.nonEmpty) {
      val v = st.head
      if (g1.adjacentVertices(v, true).isEmpty) {
        result.push(v)
        st.pop()
      }
      else {
          val e = g1.incidentEdges(v, true).head
          g1.setEdge(e, None)
          g1.setEdge((e._2, e._1), None)
          st.push(g1.edgeVertices(e)._2)
      }
    }
    Some(result)
  }

  def connected[G[_,_]: FiniteGraph, VW, EW](g: G[VW, EW], inGraph: mutable.Map[FiniteGraph[G]#V, Boolean]): Boolean = {
    val comps = DisjointSets(g.verticesNums)
    for (e <- g.edges) {
      comps.union(e._1, e._2)
    }
    var a = g.verticesNums.head
    for (v <- g.verticesNums if inGraph(v); c <- comps.find(v)) a = c
    for (v <- g.verticesNums if inGraph(v); c <- comps.find(v))
      if (a /== c)
        return false
    true
  }

  def fleury[G[_,_]: FiniteGraph, VW, EW](g: G[VW, EW]): Option[Seq[FiniteGraph[G]#V]] = {
    var current = isEulerian(g) match {
      case Cycle => g.verticesNums.head
      case Chain => g.verticesNums.filter(v => g.adjacentVertices(v, true).length % 2 === 1).head
      case NonEulerian => return None
    }

    val result: mutable.Stack[FiniteGraph[G]#V] = new mutable.Stack[FiniteGraph[G]#V]()
    val g1 = FiniteGraph[G].create(g.vertices, g.edges.map(e => ((e._1, e._2), Unit)))
    val inGraph: mutable.Map[FiniteGraph[G]#V, Boolean] = mutable.Map.empty
    for (v <- g1.verticesNums) {inGraph(v) = true}
    result.push(current)

    var cont = true
    while(cont){
      var chosen = false
      var v1 = current
      for(v  <- g1.adjacentVertices(current, true) if !chosen) {
        val g2 = FiniteGraph[G].create(g1.vertices, g1.edges.map(e => ((e._1, e._2), Unit)))
        g2.setEdge((current, v), None)
        g2.setEdge((v, current), None)
        if (g2.adjacentVertices(current, true).isEmpty)
          inGraph(current) = false

        if (connected(g2, inGraph)) {
          chosen = true
          g1.setEdge((current, v), None)
          g1.setEdge((v, current), None)
          if (g1.adjacentVertices(current, true).isEmpty)
            inGraph(current) = false
          current = v
          result.push(current)
        }
        else {
          inGraph(current) = true
        }
        v1 = v
      }
      if (!chosen) {
        g1.setEdge((current, v1), None)
        g1.setEdge((v1, current), None)
        current = v1
        result.push(current)
      }

      if(g1.adjacentVertices(current, true).isEmpty) {
        cont = false
      }
    }
    Some(result)
  }

  def prim[G[_,_]: FiniteGraph, VW, EW: Order](g: G[VW, EW]): G[VW, EW] = {
    implicit val ewo = Order[EW].toScalaOrdering  // для неявного преобразования scalaz.Order к scala Ordering

    // множество ребер результирующего MST
    val resultEdges: mutable.Set[((FiniteGraph[G]#V, FiniteGraph[G]#V), EW)] = mutable.Set.empty

    // множество вершин результирующего MST
    val treeVertices: mutable.Set[FiniteGraph[G]#V] = mutable.Set.empty
    treeVertices.add(g.verticesNums.head) // добавим в MST произвольную начальную вершину

    // для выделения вершин не из MST зададим множество всех вершин графа
    val allVertices: Set[FiniteGraph[G]#V] = g.verticesNums.toSet

    // пока MST не охватило все вершины графа
    while(treeVertices.size < allVertices.size){
      // выберем ребро минимального веса, соединяющее вершину из treeVertices с вершиной не из treeVertices
      val newEdge = (for (v1 <- treeVertices;
                          v2 <- allVertices &~ treeVertices;
                          ew <- g.edge(v1, v2))
      yield ((v1, v2), ew)).minBy(_._2)
      treeVertices.add(newEdge._1._2) // добавим вторую вершину ребра в treeVertices
      resultEdges.add(newEdge) // добавим ребро в множество ребер MST
    }

    // возвращаем граф, построенный на вершинах исходного графа ребрами из resultEdges
    FiniteGraph[G].create(g.vertices, resultEdges.toSeq)
  }

  def kruskal[G[_,_]: FiniteGraph, VW, EW: Order](g: G[VW, EW]): G[VW, EW] = {
    val resultEdges: mutable.Set[((FiniteGraph[G]#V, FiniteGraph[G]#V), EW)] = mutable.Set.empty
    val comps = DisjointSets(g.verticesNums)

    for (e <- g.edges.sortWith((e1, e2) => g.edgeWeight(e1) < g.edgeWeight(e2));
         (v1, v2) = g.edgeVertices(e)
         if comps.find(v1) /== comps.find(v2)) {
      resultEdges.add(((v1, v2), g.edgeWeight(e)))
      comps.union(v1, v2)
    }

    FiniteGraph[G].create(g.vertices, resultEdges.toSeq)
  }

  def boruvka[G[_,_]: FiniteGraph, VW, EW: Order](g: G[VW, EW]): G[VW, EW] = {
    var resultEdges: Seq[((FiniteGraph[G]#V, FiniteGraph[G]#V), EW)] = Seq()
    val comps = DisjointSets(g.verticesNums)
    val minEdges: mutable.Map[FiniteGraph[G]#V, FiniteGraph[G]#E] = mutable.Map.empty
    while (resultEdges.length < g.verticesCount - 1) {
      for (e <- g.edges;
           (v1, v2) = g.edgeVertices(e);
           c1 <- comps.find(v1);
           c2 <- comps.find(v2)
           if c1 /== c2) {
        val ew = g.edgeWeight(e)
        if (!minEdges.contains(c1) || g.edgeWeight(minEdges(c1)) > ew) {
          minEdges(c1) = e
        }
        if (!minEdges.contains(c2) || g.edgeWeight(minEdges(c2)) > ew) {
          minEdges(c2) = e
        }
      }
      for (v <- g.verticesNums;
           vComp <- comps.find(v)
           if vComp === v && minEdges.contains(v);
           e = minEdges(v)) {
        resultEdges :+= (e, g.edgeWeight(e))
        comps.union(e._1, e._2)
      }
      minEdges.clear()
    }
    FiniteGraph[G].create(g.vertices, resultEdges)
  }

  def showGraph[G[_,_]: FiniteGraph, VW, EW](g: G[VW, EW]): Unit = {
    for (e <- g.edges) {
      println((e, g.edgeWeight(e)))
    }
  }

  def sumGraph[G[_,_]: FiniteGraph, VW, EW: Monoid](g: G[VW, EW]): EW = {
    import scalaz.syntax.monoid._
    g.edges.map(g.edgeWeight).fold(Monoid[EW].zero)(_ |+| _)
  }
}