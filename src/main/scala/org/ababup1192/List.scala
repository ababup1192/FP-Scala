package org.ababup1192

// ConsとNilが共存するために、Listの型パラメータを共変にする。 3.1 「変位について」トピック
sealed trait List[+A]

// 全ての型のサブクラスであるNothingがリストの末尾(または空のリスト)の型
case object Nil extends List[Nothing]

// 頭と残りの要素 くっつける Cons
// List("a", "b") -> Cons("a", Cons("b", Nil))
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // head と tail の構造になってるので、再帰的に演算が可能
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => sys.error("Nil can't return tail")
      case Cons(h, t) => t
    }
  }

  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) list
    else list match {
      case Nil => Nil
      case (Cons(_, t)) => drop(t, n - 1)
    }
  }

  def setHead[A](list: List[A], v: A): List[A] = {
    list match {
      case Nil => sys.error("Nil can't return tail")
      case Cons(h, t) => Cons(v, t)
    }
  }


  // A*で可変長引数(Array)もConsに併せて headとtailに分けて再帰的に取り出していく。
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    // Array等を連続パラメータに渡す時は : _* と明示する必要がある。
    else Cons(as.head, apply(as.tail: _*))
  }
}
