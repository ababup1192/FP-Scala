package org.ababup1192

import org.ababup1192.datastructure._
import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  "The size function" should "return size of tree" in {
    val tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    tree.size() should be(7)
  }

  "The maximum function" should "return a maximum value of tree" in {
    val tree = Branch(Branch(Leaf(1), Leaf(10)), Branch(Leaf(5), Leaf(3)))
    tree.maximum(Math.max) should be(10)
  }

  "The depth function" should "return a longest path of tree" in {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))))
    tree.depth() should be(4)
  }

  "The map function" should "return a new Tree" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    tree.map(_ * 2) should be(Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8))))
  }

}


