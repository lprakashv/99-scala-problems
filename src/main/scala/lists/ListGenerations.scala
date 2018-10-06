package lists

import scala.util.Random

object ListGenerations {
  def range(start: Int, end: Int): List[Int] = {
    if (start == end) List(start)
    else start :: range(start+1, end)
  }

  def randomSelect[T](n: Int, list: List[T]): List[T] = Random.shuffle(list) match {
    case some if n == 0 || some == Nil => Nil
    case h :: tail => h :: randomSelect(n-1, tail)
  }

  def lotto(n: Int, limit: Int): List[Int] = randomSelect(n, range(1, limit))

  //TODO - implement without built-in methods
  def randomPermute(list: List[_]): List[_] = Random.shuffle(list)

  def combinations(i: Int, list: List[Symbol]): List[List[Symbol]] = ???
}
