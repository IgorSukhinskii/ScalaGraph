package graph.disjointSets

import scalaz.Equal
import scalaz.syntax.equal._

/**
 * Created by si1en7ium on 31.03.14.
 */
trait DisjointSets[T] {
  def add(elem: T): Unit
  def union(elem1: T, elem2: T): T
  def find(elem: T): Option[T]
}

object DisjointSets {
  def apply[T: Equal](initialElements: Seq[T]): DisjointSets[T] = new DisjointSetsImpl[T](initialElements)
}

// конструктор приватный для модуля, создание объекта должно происходить через object DisjointSets
class DisjointSetsImpl[T: Equal] private[disjointSets] (elems: Seq[T] = Seq()) extends DisjointSets[T] {
  def add(elem: T): Unit = {
    nodes += (elem -> elem)
    rank += (elem -> 0)
  }

  def union(elem1: T, elem2: T): T = {
    (for {
      a <- this.find(elem1)
      b <- this.find(elem2)
    } yield {
      if (a === b)
        a
      else if (this.rank(a) < this.rank(b)) {
        nodes(b) = a
        a
      }
      else if (this.rank(b) < this.rank(a)) {
        nodes(a) = b
        b
      }
      else {
        rank(a) += 1
        nodes(b) = a
        a
      }
    }) getOrElse(throw new MatchError("Only elements previously added to the disjoint-sets can be unioned"))
  }

  def find(elem: T): Option[T] = {
    if (!this.nodes.contains(elem))
      None
    else if (elem === this.nodes(elem))
      Some(elem)
    else {
      val e = this.find(this.nodes(elem))
      this.nodes(elem) = e getOrElse elem
      e
    }
  }

  private val nodes : scala.collection.mutable.Map[T, T] = scala.collection.mutable.Map.empty
  private val rank: scala.collection.mutable.Map[T, Int] = scala.collection.mutable.Map.empty

  //ctor
  elems foreach this.add
}
