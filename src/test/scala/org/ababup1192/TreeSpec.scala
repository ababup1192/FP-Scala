package org.ababup1192

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  "The size function" should "return size of tree" in {
    val tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    tree.size() should be(7)
  }

  "The maximum function" should "return a maximum value of tree" in {
    val tree = Branch(Branch(Leaf(1), Leaf(10)), Branch(Leaf(5), Leaf(3)))
    Tree.maximum(tree) should be(10)
  }

  "The depth function" should "return a longest path of tree" in {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))))
    tree.depth() should be(4)
  }

}


