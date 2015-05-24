package org.ababup1192

sealed trait Tree[+A] {
  def size(): Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size() + r.size()
  }

  def depth(): Int = {
    this match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (l.depth() max r.depth())
    }
  }

}

object Tree {
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }


}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

