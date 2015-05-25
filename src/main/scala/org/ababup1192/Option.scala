package org.ababup1192


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f(_)).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(a) => a
      case None => default
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap { a =>
      if (f(a)) Some(a)
      else None
    }
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    age * numberOfSpeedingTickets * 0.1
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(va => b.map(vb => f(va, vb)))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = {
    _ map f
  }

  def sequence[A](list: List[Option[A]]): Option[List[A]] = {
    Some(List.foldLeft(list, List(): List[A])((l, r) => r match {
      case None => l
      case Some(x) => List.append(l, List(x))
    }))
  }

  def parseInts(list: List[String]): Option[List[Int]] = {
    traverse(list)(a => Try(a.toInt))
  }

  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = {
    Some(List.foldLeft(list, List(): List[B])((l, r) => {
      f(r) match {
        case None => l
        case Some(x) => List.append(l, List(x))
      }
    }))
  }

}
