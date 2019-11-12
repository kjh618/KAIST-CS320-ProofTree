package prooftree

import scala.sys.error
import scala.util.parsing.combinator._

// Parser for CFWAE
object ExprParser extends RegexParsers {
  def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"

  lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)

  lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r

  lazy val expr: Parser[Expr] =
    int                                       ^^ { case n => Num(n) }                        |
    wrap("+" ~> expr ~ expr)                  ^^ { case l ~ r => Add(l, r) }                 |
    wrap("-" ~> expr ~ expr)                  ^^ { case l ~ r => Sub(l, r) }                 |
    wrap("*" ~> expr ~ expr)                  ^^ { case l ~ r => Mul(l, r) }                 |
    wrap("with" ~> wrap(str ~ expr) ~ expr)   ^^ { case x ~ i ~ b => With(x, i, b) }         |
    str                                       ^^ { case x => Id(x) }                         |
    wrap("fun" ~> wrap(str) ~ expr)           ^^ { case p ~ b => Fun(p, b) }                 |
    wrap(expr ~ expr)                         ^^ { case f ~ a => App(f, a) }                 |
    wrap("if0" ~> expr ~ expr ~ expr)         ^^ { case c ~ t ~ f => If0(c, t, f) }

  def apply(str: String): Expr = parseAll(expr, str).getOrElse(error(s"bad syntax: $str"))
}