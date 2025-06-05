package de.professionalowo
package parser

import parser._

private def bind[A, B](a: Parser[A], f: A => Parser[B]): Parser[B] =
  inp => a(inp).flatMap { case (value, remaining) => f(value)(remaining) }

def many[A](p: => Parser[A]): Parser[List[A]] =
  (p >>= { x => many(p).map { xs => x :: xs } }) ++ result(Nil)

def many1[A](p: => Parser[A]): Parser[List[A]] =
  (p >>= { x => many(p).map { xs => x :: xs } })

def many1C(p: => Parser[Char]): Parser[String] = c(many1(p))

def manyC(p: => Parser[Char]): Parser[String] = c(many(p))

private def c(p: => Parser[List[Char]]): Parser[String] =
  inp => p(inp).map { case (list, remaining) => (list.mkString, remaining) }

def sepby1[A, B](p: => Parser[A], sep: => Parser[B]): Parser[List[A]] =
  p >>= { x => many(sep >>= { _ => p }).map { xs => x :: xs } }

def sepby[A, B](p: => Parser[A], sep: => Parser[B]): Parser[List[A]] =
  sepby1(p, sep) ++ result(Nil)

def bracket[A, B, C](open: => Parser[A], p: => Parser[B], close: => Parser[C]): Parser[B] =
  open >>= { _ =>
    p >>= { x =>
      close >>= { _ =>
        result(x)
      }
    }
  }

def parse[A](p: => Parser[A]): Parser[A] =
  junk() >>= { _ => p }