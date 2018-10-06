package lists

import scala.collection.mutable

object ListTransformations {
  def reverse(list: List[Int]): List[Int] = list.foldLeft(List.empty[Int]) { (acc, i) =>
    i :: acc
  }

  def flatten(list: List[Any]): List[Int] = list.flatMap {
    case i: Int => List(i)
    case l: List[Int] => l
    case other: List[Any] => flatten(other)
    case _ => throw new Exception("INCONSISTENT_DATA_TYPE")
  }

  def compress(list: List[Symbol]): List[Symbol] = {
    val buff = mutable.LinkedHashSet.empty[Symbol]
    list.map(buff += _)
    buff.toList
  }

  def pack(list: List[Symbol]): List[List[Symbol]] = list.foldLeft(List.empty[List[Symbol]]) {
    case (Nil, elem) => List(List(elem))
    case (packed :: tail, elem) if packed.nonEmpty && packed.head == elem => (elem :: packed) :: tail
    case (packedList, elem) => List(elem) :: packedList
  }.reverse

  def encode(list: List[Symbol]): List[(Int, Symbol)] = list.foldLeft(List.empty[(Int, Symbol)]) {
    case ((currHeadCount, head) :: tail, elem) if head == elem => (currHeadCount + 1, head) :: tail
    case (acc, elem) => (1, elem) :: acc
  }.reverse

  def decode(list: List[(Int, Symbol)]): List[Symbol] = {
    def times(i: Int, s: Symbol): List[Symbol] = if (i == 0) Nil else s :: times(i-1, s)
    list.foldLeft(List.empty[Symbol]) {
      case (acc, (i, s)) => acc ::: times(i, s)
    }
  }

  def duplicate(list: List[Symbol]): List[Symbol] = list flatMap {
    s => List(s, s)
  }

  def duplicateN(i: Int, list: List[Symbol]): List[Symbol] = {
    def times(i: Int, s: Symbol): List[Symbol] = if (i == 0) Nil else s :: times(i-1, s)
    list flatMap {
      s => times(i, s)
    }
  }

  def drop(i: Int, list: List[Symbol]): List[Symbol] = list.foldLeft((1, List.empty[Symbol])) {
    case ((count, acc), s) if count == i => (1, acc)
    case ((count, acc), s) => (count + 1, s :: acc)
  }._2.reverse


  //TODO - implement
  def rotate(i: Int, list: List[Symbol]): List[Symbol] = {
    def fun(r: Int, ls: List[Symbol]): List[Symbol] = ls match {
      case Nil => Nil
      case some if r == 0 => some
      case h :: tail => fun(r-1, tail  ::: h :: Nil)
    }
    if (i>0) fun(i%list.length, list)
    else fun(list.length + (i % list.length), list)
  }
}
