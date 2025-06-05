package de.professionalowo
package parser

abstract class Parser[+A] extends (String => List[(A, String)]) {
  def >>=[B](f: A => Parser[B]): Parser[B] = bind(this, f)

  def flatMap[B](f: A => Parser[B]): Parser[B] = this >>= f

  def ++[B >: A](other: => Parser[B]): Parser[B] =
    input => this.apply(input) ::: other.apply(input)

  def map[B](f: A => B): Parser[B] =
    input => this.apply(input).map { case (value, remaining) =>
      (f(value), remaining)
    }
}

def result[A](v: => A): Parser[A] =
  inp => List((v, inp))

def zero[A](): Parser[A] =
  _ => Nil

def item(): Parser[Char] = {
  case s if s.nonEmpty => List((s.head, s.tail))
  case _ => Nil
}

