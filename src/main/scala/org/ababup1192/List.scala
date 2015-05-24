package org.ababup1192

// ConsとNilが共存するために、Listの型パラメータを共変にする。 3.1 「変位について」トピック
sealed trait List[+A]

// 全ての型のサブクラスであるNothingがリストの末尾(または空のリスト)の型
case object Nil extends List[Nothing]

// 頭と残りの要素 くっつける Cons
// List("a", "b") -> Cons("a", Cons("b", Nil))
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {


  def foldRight[A, B](list: List[A], res: B)(f: (A, B) => B): B = {
    list match {
      case Nil => res
      case Cons(x, xs) => f(x, foldRight(xs, res)(f))
    }
  }

  def foldLeft[A, B](list: List[A], res: B)(f: (B, A) => B): B = {
    list match {
      case Nil => res
      case Cons(x, xs) => foldLeft(xs, f(res, x))(f)
    }
  }

  def length[A](list: List[A]): Int = {
    foldLeft(list, 0)((l, _) => l + 1)
  }

  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, List(): List[A])((l, r) => Cons(r, l))
  }

  // head と tail の構造になってるので、再帰的に演算が可能
  def sum(list: List[Int]): Int = {
    foldLeft(list, 0)(_ + _)
  }

  def product(list: List[Double]): Double = {
    foldLeft(list, 1.0)(_ * _)
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
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = {
    list match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => list
    }
  }

  def concat[A](lists: List[List[A]]): List[A] = {
    foldLeft(lists, List(): List[A])((l, r) => append(l, r))
  }

  def append[A](list1: List[A], list2: List[A]): List[A] = {
    foldRight(list1, list2)((l, r) => Cons(l, r))
  }

  def plusOne(list: List[Int]): List[Int] = {
    foldRight(list, List(): List[Int])((l, r) => Cons(l + 1, r))
  }



  def init[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
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
