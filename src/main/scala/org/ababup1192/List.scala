package org.ababup1192

// Listは共変 要素に継承関係があれば、それが適用される。
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

  // A*で可変長引数(Array)もConsに併せて headとtailに分けて再帰的に取り出していく。
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    // Array等を連続パラメータに渡す時は : _* と明示する必要がある。
    else Cons(as.head, apply(as.tail: _*))
}
