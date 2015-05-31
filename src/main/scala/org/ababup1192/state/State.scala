package org.ababup1192.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    val int: Rand[Int] = _.nextInt

    def nonNegativeInt: Rand[Int] = {
      map(int)(i => if (i < 0) -(i + 1) else i)
    }

    def nonNegativeEven: Rand[Int] = {
      map(nonNegativeInt)(i => i - i % 2)
    }

    def double: Rand[Double] = {
      val (n, r) = nonNegativeInt(this)
      rng: RNG => (n / (Int.MaxValue.toDouble + 2), r)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r) = rng.nextInt
      val (d, r2) = double(r)
      (i -> d, r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (v, r) = intDouble(rng)
      (v.swap, r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      import org.ababup1192.stream._
      val list = Stream.unfold(rng.nextInt) {
        case (n, rng: RNG) => Some(n -> rng, rng.nextInt)
      }.take(count).toList
      val (_, r) = list.last
      (list.map(_._1), r)
    }


  }


}


