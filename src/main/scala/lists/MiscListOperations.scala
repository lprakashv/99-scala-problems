package lists

import java.util.NoSuchElementException

import scala.annotation.tailrec

object MiscListOperations {
  @tailrec
  def penultimate(list: List[Int]): Option[Int] = list match {
    case pen :: _ :: Nil => Some(pen)
    case _ :: tail => penultimate(tail)
    case Nil => None
  }

  def nth(n: Int, list: List[Int]): Option[Int] = {
    val nTaken = list.take(n+1)
    if (nTaken.length == n + 1) {
      Some(nTaken.last)
    } else None
  }

  def length(list: List[Int]): Int = list.foldLeft(0) { (acc, _) =>
    acc + 1
  }

  def split(i: Int, list: List[Symbol]): (List[Symbol], List[Symbol]) = {
    def tailPartAfter(p: Int, ls: List[Symbol]): List[Symbol] = ls match {
      case Nil => Nil
      case h :: tail if p == 1 => tail
      case h :: tail => tailPartAfter(p-1, tail)
    }
    (list.take(i), tailPartAfter(i, list))
  }

  def slice(start: Int, end: Int, list: List[Symbol]): List[Symbol] = {
    def tailPartAfter(p: Int, ls: List[Symbol]): List[Symbol] = ls match {
      case Nil => Nil
      case h :: tail if p == 1 => tail
      case h :: tail => tailPartAfter(p-1, tail)
    }
    tailPartAfter(start, list).take(end - start)
  }

  def removeAt(k: Int, list: List[Symbol]): (List[Symbol], Symbol) = {
    val zipped = list.zipWithIndex
    val kth = zipped.collectFirst {
      case (s, i) if i== k => s
    }.getOrElse(throw new NoSuchElementException)
    (zipped.filterNot(i => i._2 == k).map(_._1), kth)
  }

  def insertAt(n: Symbol, i: Int, list: List[Symbol]): List[Symbol] = list match {
    case ls if i == 0 || ls == Nil => n :: ls
    case h :: tail => h :: insertAt(n, i-1, tail)
  }
}
