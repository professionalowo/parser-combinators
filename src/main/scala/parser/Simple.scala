package de.professionalowo
package parser

def sat(p: Char => Boolean): Parser[Char] =
  item() >>= { x => if (p(x)) result(x) else zero() }

def char(x: Char): Parser[Char] = sat {
  x == _
}

def digit(): Parser[Char] = sat {
  _.isDigit
}

def lower(): Parser[Char] = sat {
  _.isLower
}

def upper(): Parser[Char] = sat {
  _.isUpper
}

def letter(): Parser[Char] = lower() ++ upper()

def alphanum(): Parser[Char] = letter() ++ digit()

def string(s: String): Parser[String] =
  if (s.isEmpty) result(s)
  else char(s.head) >>= { _ => string(s.tail) >>= { _ => result(s) } }

def word(): Parser[String] =
  (letter() >>= { x => word() >>= { xs => result(x.toString + xs) } }) ++ result("")

def spaces(): Parser[Unit] = many1(sat {
  _.isSpaceChar
}) >>= { _ => result(()) }


def junk(): Parser[Unit] = many(spaces()).map(_ => ())