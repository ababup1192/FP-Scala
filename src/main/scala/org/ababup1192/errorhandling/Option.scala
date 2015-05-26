package org.ababup1192.errorhandling

import org.ababup1192.datastructure


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
    for {
      va <- a
      vb <- b
    } yield f(va, vb)
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = {
    _ map f
  }

  def sequence[A](list: datastructure.List[Option[A]]): Option[datastructure.List[A]] = {
    Some(datastructure.List.foldLeft(list, datastructure.List(): datastructure.List[A])((l, r) => r match {
      case None => l
      case Some(x) => datastructure.List.append(l, datastructure.List(x))
    }))
  }

  def parseInts(list: datastructure.List[String]): Option[datastructure.List[Int]] = {
    traverse(list)(a => Try(a.toInt))
  }

  def traverse[A, B](list: datastructure.List[A])(f: A => Option[B]): Option[datastructure.List[B]] = {
    Some(datastructure.List.foldLeft(list, datastructure.List(): datastructure.List[B])((l, r) => {
      f(r) match {
        case None => l
        case Some(x) => datastructure.List.append(l, datastructure.List(x))
      }
    }))
  }

}
