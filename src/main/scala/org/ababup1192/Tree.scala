package org.ababup1192

sealed trait Tree[+A] {

  def fold[B](fl: A => B)(fb: (B, B) => B): B = {
    this match {
      case Leaf(v) => fl(v)
      case Branch(l, r) => fb(l.fold(fl)(fb), r.fold(fl)(fb))
    }
  }

  def size(): Int = {
    this.fold(a => 1)(1 + _ + _)
  }

  def depth(): Int = {
    this.fold(a => 0)((l, r) => 1 + (l max r))
  }

  def map[B](f: A => B): Tree[B] = {
    this.fold(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }

  def maximum[B >: A](f: (B, B) => B): B = {
    this.fold((a: B) => a)(f(_, _))
  }
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

