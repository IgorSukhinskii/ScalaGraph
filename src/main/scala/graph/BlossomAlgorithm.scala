package graph
import scalaz.Equal
import scalaz.syntax.equal._
import scalaz.std.AllInstances._

import graph.syntax.all._
import scala.collection.mutable

trait BlossomAlgorithm {
  def blossomAlgorithm[G[_,_]: FiniteGraph, VW, EW](g: G[VW, EW]): G[Unit, Unit] = {
    val result: G[Unit, Unit] = FiniteGraph[G].createNonWeighted(g.verticesNums, Seq())

    while(true) {
      val path = findAlternatingPath(g, result)
      path match {
        case Some(p) => updateMatching(result, p)
        case None    => return result
      }
    }

    result
  }

  private def updateMatching[G[_,_]: FiniteGraph](m: G[Unit, Unit], path: Seq[FiniteGraph[G]#V]): Unit = {
    for (i <- Range(0, path.length - 1)) {
      m.edgeNum(path(i), path(i+1)) match {
        case Some((v1, v2)) => {
          m.setEdge((v1, v2), None)
          m.setEdge((v2, v1), None)
        } // удалить ребро из паросочетания, если оно в нём есть
        case None => {
          m.setEdge((path(i), path(i+1)), Some(Unit))
          m.setEdge((path(i+1), path(i)), Some(Unit))
        } // добавить, если ребра нет
      }
    }
  }

  // определим вспомогательный класс для хранения леса
  private case class NodeInfo[V](parent: Option[V], treeRoot: V, isOuter: Boolean)

  private type Edge[T] = (T, T) //определим синоним типа для ребра

  private case class Blossom[V](root: V, cycle: Seq[V], nodes: Set[V])

  private type Forest[V] = Map[V, NodeInfo[V]]

  private def findAlternatingPath[G[_,_]: FiniteGraph, VW1, EW1, VW2, EW2](g: G[VW1, EW1],
                                                                      m: G[VW2, EW2]): Option[Seq[FiniteGraph[G]#V]] = {

    val forest:   mutable.Map[FiniteGraph[G]#V, NodeInfo[FiniteGraph[G]#V]] = mutable.Map.empty
    val worklist: mutable.Queue[Edge[FiniteGraph[G]#V]] = mutable.Queue.empty

    for (node <- g.verticesNums if m.adjacentVertices(node, true).isEmpty) {
      forest(node) = NodeInfo(None, node, isOuter = true)
      for (e <- g.incidentEdges(node, true)) {
        worklist.enqueue(e)
      }
    }

    while(!worklist.isEmpty) {
      val curr = worklist.dequeue()
      if (!m.edge(curr._1, curr._2).isDefined) {
        val startInfo = forest(curr._1)
        val end   = forest.get(curr._2)

        end match {
          case Some(endInfo) => {
            if (endInfo.isOuter && (startInfo.treeRoot === endInfo.treeRoot)) {
              val blossom = findBlossom(forest.toMap, curr)
              return findAlternatingPath(contractGraph[G, VW1, EW1](g, blossom),
                                         contractGraph[G, VW2, EW2](m, blossom)) map
                (path => expandPath(g, path, forest.toMap, blossom))
            }
            else if (endInfo.isOuter && (startInfo.treeRoot /== endInfo.treeRoot)) {
              var result: Vector[FiniteGraph[G]#V] = Vector()
              var node: Option[FiniteGraph[G]#V] = Some(curr._1)
              while (node /== None) {
                result +:= node.get
                node = forest(node.get).parent
              }
              node = Some(curr._2)
              while (node /== None) {
                result :+= node.get
                node = forest(node.get).parent
              }
              return Some(result)
            }
          }
          case None => {
            forest(curr._2) = NodeInfo(Some(curr._1), startInfo.treeRoot, isOuter = false)
            val endpoint: FiniteGraph[G]#V = m.adjacentVertices(curr._2, true).head
            forest(endpoint) = NodeInfo(Some(curr._2), startInfo.treeRoot, isOuter = false)
            for (v <- g.adjacentVertices(endpoint, true)) {
              worklist.enqueue((endpoint, v))
            }
          }
        }
      }
    }

    None
  }

  private def findBlossom[V: Equal](forest: Forest[V], edge: Edge[V]): Blossom[V] = {
    var onePath: Vector[V] = Vector()
    var twoPath: Vector[V] = Vector()
    var node: Option[V] = Some(edge._1)
    while (node =/= None){
      onePath +:= node.get
      node = forest(node.get).parent
    }
    node = Some(edge._2)
    while (node =/= None){
      twoPath +:= node.get
      node = forest(node.get).parent
    }

    var mismatch: Int = 0
    while (mismatch < onePath.length && mismatch < twoPath.length && onePath(mismatch) === twoPath(mismatch))
      mismatch+=1

    var cycle: Vector[V] = Vector()
    for (i <- mismatch - 1 until onePath.length)
      cycle :+= onePath(i)

    for (i <- twoPath.length - 1 to mismatch - 1 by -1)
      cycle :+= twoPath(i)


    val s: Set[V] = cycle.toSet
    Blossom(onePath(mismatch - 1), cycle, s)
  }

  private def contractGraph[G[_,_]: FiniteGraph, VW, EW](g: G[VW, EW], blossom: Blossom[FiniteGraph[G]#V]): G[VW, EW] ={
    val result: G[VW, EW] = FiniteGraph[G].create(Seq(), Seq())
    for (v <- g.verticesNums if !blossom.nodes.contains(v))
      result.setVertex(v, Some(g.vertices(v)))
    result.setVertex(blossom.root, Some(g.vertices(blossom.root)))

    for (v0 <- g.verticesNums if !blossom.nodes.contains(v0); v1 <- g.adjacentVertices(v0, true)) {
      if (blossom.nodes.contains(v1))
        result.setEdge((v0, blossom.root), g.edge(v0, v1))
      else
        result.setEdge((v0, v1), g.edge(v0, v1))
    }
    result
  }

  private def expandPath[G[_,_]: FiniteGraph, VW, EW](g: G[VW, EW],
                                                 p: Seq[FiniteGraph[G]#V],
                                                 forest: Forest[FiniteGraph[G]#V],
                                                 blossom: Blossom[FiniteGraph[G]#V]): Seq[FiniteGraph[G]#V] = {
    var path = p
    val index = path.indexOf(blossom.root)
    if (index === -1) return path
    if (index % 2 === 1)
      path = path.reverse
    var result: Vector[FiniteGraph[G]#V] = Vector()
    for (i <- 0 until path.length) {
      if (path(i) /== blossom.root) {
        result :+= path(i)
      }
      else {
        result :+= blossom.root
        val outNode = findNodeLeavingCycle(g, blossom, path(i + 1))
        val outIndex = blossom.cycle.indexOf(outNode)
        val start = if (outIndex % 2 === 0) 1 else blossom.cycle.length - 2
        val step  = if (outIndex % 2 === 0) +1 else -1
        for (k <- start to outIndex by step)
          result :+= blossom.cycle(k)
      }
    }
    result
  }

  private def findNodeLeavingCycle[G[_,_]: FiniteGraph, VW, EW](g: G[VW, EW],
                                                           blossom: Blossom[FiniteGraph[G]#V],
                                                           node: FiniteGraph[G]#V): FiniteGraph[G]#V = {
    for (cycleNode <- blossom.nodes if g.edge(cycleNode, node).isDefined)
      return cycleNode
    throw new AssertionError("Could not find an edge out of the blossom.")
  }
}
