package lambdamu

import org.scalatest.funsuite.AnyFunSuite
import lambdamu.{Expr, Evaluator, Parser, PrettyPrinter}

class EvaluatorSpec extends AnyFunSuite:

  def parse(input: String): Expr = Parser.parse(input)
  def eval(input: String): Expr = Evaluator.reduce(Evaluator.resolve(parse(input)))
  def evalPretty(input: String): String = PrettyPrinter.pretty(eval(input))

  test("Variable resolves to itself") {
    assert(evalPretty("x") == "x")
  }

  test("Lambda abstraction stays intact") {
    assert(evalPretty("λx.x") == "λx.x")
  }

  test("Application of identity") {
    assert(evalPretty("(λx.x) y") == "y")
  }

  test("Nested application") {
    assert(evalPretty("(λx.λy.x) a b") == "a")
  }

  test("Mu-app reduction") {
    val result = evalPretty("(μa.[a]x) y")
    assert(result == "μa.[a]x y")
  }

  test("Freeze-mu cancel") {
    val result = evalPretty("[a](μa.x)")
    assert(result == "x")
  }

  test("Double freeze reduces") {
    val result = evalPretty("[a][a]x")
    assert(result == "[a]x")
  }

  test("Mu-freeze cancel") {
    val result = evalPretty("μa.[a]x")
    assert(result == "x")
  }

  test("Let definition and resolution") {
    val xDef = parse("λy.y")
    Evaluator.env("id") = xDef
    assert(evalPretty("id z") == "z")
  }

  test("Pretty printing structure") {
    val expr = parse("(λx.x) (λy.y)")
    val pretty = PrettyPrinter.pretty(expr)
    assert(pretty.startsWith("(λx.x)"))
  }

  test("Parsing failure throws") {
    assertThrows[IllegalArgumentException] {
      LambdaParser.parse("λx.")
    }
  }

