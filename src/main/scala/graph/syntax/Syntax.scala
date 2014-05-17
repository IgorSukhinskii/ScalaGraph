package graph.syntax

/**
 * Created by si1en7ium on 30.03.14.
 */

// трейт "собирает" в себя все неявные преобразования, предназначенные для добавления
// новых методов к объектам-графам
trait Syntax {
  object finiteGraph extends ToFiniteGraphOps

  object all extends ToFiniteGraphOps
}
