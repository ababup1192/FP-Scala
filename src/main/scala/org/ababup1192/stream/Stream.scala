package org.ababup1192.stream

trait Stream[+A] {
  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }
  }

  def toList: List[A] = {
    def loop(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case _ => acc
    }
    loop(this, List()).reverse
  }

  def take(n: Int): Stream[A] = {
    def loop(n: Int, stream: Stream[A]): Stream[A] = stream match {
      case _ if n <= 0 => Stream.empty
      case Cons(h, t) => Stream.cons(h(), loop(n - 1, t()))
    }
    loop(n, this)
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }
}