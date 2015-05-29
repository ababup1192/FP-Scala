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

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def map[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => f(h).append(t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => if (f(h)) Stream.cons(h, t) else t)
  }

  def append[B >: A](stream: => Stream[B]): Stream[B] = {
    foldRight(stream)((h, t) => Stream.cons(h, t))
  }

  def take(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (Stream.empty, 0)))
      case (Cons(h, t), nn) if nn > 1 => Some((h(), (t(), nn - 1)))
      case _ => None
    }
  }

  def find(p: A => Boolean): Option[A] = {
    filter(p).headOption
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def dropWhile(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((h, t) => if (p(h)) t else Stream.cons(h, t))
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((h, t) => p(h) && t)
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((h, t) => p(h) || t)
  }

  def zipWith[B, C](stream: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, stream)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)(_ -> _)

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), None) ->(t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) => Some(f(None, Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
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

  def constant[A](a: A): Stream[A] = {
    unfold(a)(_ => Some((a, a)))
  }

  def from(n: Int): Stream[Int] = {
    unfold(n)(s => Some((s, s + 1)))
  }

  def fib(n: Int): Stream[Int] = {
    unfold((0, 1)) {
      case (f0, f1) => Some((f0, (f1, f0 + f1)))
    }.take(n)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

}