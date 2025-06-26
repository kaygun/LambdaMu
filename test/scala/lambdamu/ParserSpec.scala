package lambdamu

import org.scalatest.funsuite.AnyFunSuite
import lambdamu.{Expr,Parser}

class ParserSpec extends AnyFunSuite:

  def parse(input: String): Expr = Parser.parse(input)

  test("Parse variable") {
    assert(parse("x") == Expr.Var("x"))
  }

  test("Parse lambda abstraction") {
    val expected = Expr.Lam("x", Expr.Var("x"))
    assert(parse("λx.x") == expected)
    assert(parse("\\x.x") == expected)
  }

  test("Parse mu abstraction") {
    val expected = Expr.Mu("a", Expr.Var("x"))
    assert(parse("μa.x") == expected)
    assert(parse("mu a.x") == expected)
  }

  test("Parse freeze expression") {
    val expected = Expr.Freeze("a", Expr.Var("x"))
    assert(parse("[a]x") == expected)
  }

  test("Parse application of two variables") {
    val expected = Expr.App(Expr.Var("x"), Expr.Var("y"))
    assert(parse("x y") == expected)
  }

  test("Parse nested application") {
    val expected = Expr.App(Expr.App(Expr.Var("x"), Expr.Var("y")), Expr.Var("z"))
    assert(parse("x y z") == expected)
  }

  test("Parse application with parentheses") {
    val expected = Expr.App(Expr.Var("x"), Expr.App(Expr.Var("y"), Expr.Var("z")))
    assert(parse("x (y z)") == expected)
  }

  test("Parse complex lambda and mu") {
    val input = "λx.μa.[a](λy.y)"
    val inner = Expr.Lam("y", Expr.Var("y"))
    val freeze = Expr.Freeze("a", inner)
    val mu = Expr.Mu("a", freeze)
    val lam = Expr.Lam("x", mu)
    assert(parse(input) == lam)
  }

  test("Fail on malformed expression") {
    assertThrows[IllegalArgumentException] {
      parse("λx")
    }
  }

