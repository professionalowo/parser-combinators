package de.professionalowo
package json

import parser.{Parser, alphanum, bracket, char, many1C, parse, sepby, string}

import scala.util.Try

private def ws[A](p: => Parser[A]): Parser[A] = parse(p)

private lazy val lBracket = ws(char('{'))
private lazy val rBracket = ws(char('}'))

private lazy val quote = ws(char('"'))
private lazy val colon = ws(char(':'))
private lazy val comma = ws(char(','))

private lazy val lParen = ws(char('['))
private lazy val rParen = ws(char(']'))

private lazy val stringLiteral = ws(many1C(alphanum()))

object JSON {
  private lazy val pNull = ws(parseNull());
  private lazy val pString = ws(parseString());
  private lazy val pObject = ws(parseObject());
  private lazy val pArray = ws(parseArray());
  private lazy val pBool = ws(parseBoolean())
  private lazy val pPair = ws(parsePair());

  private def parseNull(): Parser[JsonNull] = string("null").map {
    _ => JsonNull()
  }

  private def parseString(): Parser[JsonString] = ws(bracket(quote, stringLiteral, quote))
    .map(d => JsonString(d))

  private def parsePair(): Parser[(JsonString, JSON)] = for {
    key <- pString
    _ <- colon
    value <- parseJson()
  } yield (key, value)

  private def parseObject(): Parser[JsonObject] = {
    bracket(lBracket, sepby(pPair, comma)
      .map { x =>
        JsonObject(x.foldLeft(Map[JsonString, JSON]()) { (m, t) => m + (t._1 -> t._2) })
      }, rBracket)

  }

  private def parseArray(): Parser[JsonArray] =
    bracket(lParen, sepby(parseJson(), comma).map(JsonArray(_)), rParen)

  private def parseBoolean(): Parser[JsonBoolean] =
    (string("true") ++ string("false")).map { v => JsonBoolean(Try(v.toBoolean).getOrElse(false)) }

  private def parseJson(): Parser[JSON] = pNull ++
    pString ++
    pObject ++
    pBool ++
    pArray

  def parse(s: String): JSON = {
    parseJson()(s).headOption match {
      case Some((json, "")) => json
      case _ => throw new Exception("Could not parse JSON")
    }
  }
}

