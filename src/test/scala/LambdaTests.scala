import org.scalatest.funsuite.AnyFunSuite
import LambdaParser._
import Evaluator._
import scala.util.Try

class LambdaMuTestSuite extends AnyFunSuite:

  def parse(input: String): Expr =
    parseExpr(input) match
      case Right(expr) => expr
      case Left(err) => fail(err)

  def eval(input: String): Expr =
    val parsed = parse(input)
    evalExpr(parsed)

  test("Parsing variables"):
    val expr = parse("x")
    assert(expr == Var("x"))

  test("Parsing lambda expressions"):
    val expr = parse("\\x. x")
    assert(expr == Lam(Var("x"), Var("x")))

  test("Parsing mu expressions"):
    val expr = parse("#k. k")
    assert(expr == Mu(Var("k"), Var("k")))

  test("Parsing continuation expressions"):
    val expr = parse("[k] x")
    assert(expr == Cont(Var("k"), Var("x")))

  test("Application left-associative"):
    val expr = parse("x y z")
    assert(expr == Appl(Appl(Var("x"), Var("y")), Var("z")))

  test("Beta reduction of lambda"):
    val expr = parse("(\\x. x) y")
    val reduced = evalExpr(expr)
    assert(reduced == Var("y"))

  test("Mu reduction with matching variable"):
    val expr = parse("[k] #k. x")
    val reduced = evalExpr(expr)
    assert(reduced == Var("x"))

  test("Nested mu reduction"):
    val expr = parse("[k] [k] #k. x")
    val reduced = evalExpr(expr)
    assert(reduced == Var("x"))

  test("Lambda nested reductions"):
    val expr = parse("(\\x. (\\y. x)) z")
    val reduced = evalExpr(expr)
    assert(reduced == Lam(Var("y"), Var("z")))

  test("Free variable detection"):
    val expr = Lam(Var("x"), Appl(Var("x"), Var("y")))
    assert(expr.freeVars == Set(Var("y")))

  test("Alpha equivalence"):
    val e1 = parse("\\x. x")
    val e2 = parse("\\y. y")
    assert(e1.alphaEq(e2))

  test("Substitution with capture avoidance"):
    val expr = Lam(Var("x"), Lam(Var("y"), Var("x")))
    val substExpr = expr.subst(Map("x" -> Var("z")), Map.empty)
    assert(substExpr == Lam(Var("x"), Lam(Var("y"), Var("x")))) // no capture occurs

  test("Parsing failure produces Left"):
    val res = parseExpr("\\. x")
    assert(res.isLeft)


