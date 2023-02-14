package com.evolutiongaming.tests.four

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

object DataStructuresHomework {
  // Implement a special sort which sorts the keys of a map (K) according to their associated
  // values (V).
  //
  // In case of "ties" (equal values) it should group these keys K into Set-s in the results.
  //
  // The input is a map where keys (K) are the values to be sorted and values are their associated numeric
  // values.
  //
  // The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
  // with values from K, and the associated value V for these values in the `Set`.
  //
  // For example:
  //
  // Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
  // output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    val resultMap = ListBuffer.empty[(Set[T], Int)]
    ListMap(map.toSeq.sortWith(_._2 < _._2): _*).groupBy(_._2).foreach(e => {
      val list = ListBuffer.empty[T]
      e._2.foreach {
        case (key, _) => list += key
      }
      resultMap += (list.toSet -> e._1)
    })
    resultMap.toList
  }
}
