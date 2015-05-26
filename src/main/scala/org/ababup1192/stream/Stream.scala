package org.ababup1192.stream

trait Stream[+A] {
  def headOption: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  def toList: List[A] = {
    def loop(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case _ => acc
    }
    loop(this, List()).reverse
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def dropWhile(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => if (p(h)) t else Stream.cons(h, t))
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Stream.empty)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((h, t) => p(h) && t)
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((h, t) => p(h) || t)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
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