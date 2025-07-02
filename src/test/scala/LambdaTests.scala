import org.scalatest.funsuite.AnyFunSuite

class LambdaMuTest extends AnyFunSuite:

  def parse(s: String): Expr = LambdaParser.parseExpr(s) match
    case Right(e) => e
    case Left(err) => fail(s"Parser error: $err")

  test("Parser - basic lambda and variable") {
    assert(parse("x") == Var("x"))
    assert(parse("\\x.x") == Lam(Var("x"), Var("x")))
  }

  test("Parser - mu and continuation") {
    assert(parse("#a.a") == Mu(Var("a"), Var("a")))
    assert(parse("[a] x") == Cont(Var("a"), Var("x")))
  }

  test("Alpha equivalence - lambda") {
    val e1 = parse("\\x.x")
    val e2 = parse("\\y.y")
    assert(e1.alphaEq(e2))
  }

  test("Alpha equivalence - mu") {
    val e1 = parse("#a.a")
    val e2 = parse("#b.b")
    assert(e1.alphaEq(e2))
  }

  test("Substitution - avoid capture") {
    val x = Var("x")
    val y = Var("y")
    val lam = Lam(x, Appl(x, y))
    val result = Substitution(x, y, lam)
    assert(result.isInstanceOf[Lam])
    assert(!result.freeVars.contains(x))
  }

  test("Evaluator - beta reduction") {
    val expr = parse("(\\x.x) y")
    val result = Evaluator.evalSteps(expr).last
    assert(result == Var("y"))
  }

  test("Evaluator - mu application") {
    val expr = parse("(#a.[a]x) y")
    val steps = Evaluator.evalSteps(expr)
    assert(steps.last == Appl(Var("y"), Var("x")))
  }

  test("Evaluator - continuation collapse") {
    val expr = parse("[a][a]x")
    val steps = Evaluator.evalSteps(expr)
    assert(steps.last == parse("[a]x"))
  }

  test("Evaluator - mu a [a] x → x") {
    val expr = parse("#a.[a]x")
    val steps = Evaluator.evalSteps(expr)
    assert(steps.last == Var("x"))
  }

  test("Evaluator - normalize nested lambdas") {
    val expr = parse("\\x.\\y.x")
    val steps = Evaluator.evalSteps(expr)
    assert(steps.last.alphaEq(expr))
  }

  test("Parser - nested applications") {
    val expr = parse("x y z")
    assert(expr == Appl(Appl(Var("x"), Var("y")), Var("z")))
  }

  test("Evaluator - double negation elimination") {
    val dne = parse("\\f. #a. f (\\x. [a] x)")
    val notNotA = parse("\\k. k x") // a mock (¬¬A): λk. k x
    val app = Appl(dne, notNotA)
    val result = Evaluator.evalSteps(app).last
    assert(result == Var("x"))
  }

  test("Evaluator - continuation nested in mu application") {
    val expr = parse("(#a.\\x.[a]x) y")
    val steps = Evaluator.evalSteps(expr)
    assert(steps.last == Lam(Var("x"), Appl(Var("y"), Var("x"))))
  }

  test("Evaluator - continuation abstraction") {
    val expr = parse("[a](#a.x)")
    val steps = Evaluator.evalSteps(expr)
    assert(steps.last == Var("x"))
  }

  test("Evaluator - mismatched continuation label should not reduce") {
    val expr = parse("[b](#a.x)")
    val steps = Evaluator.evalSteps(expr)
    assert(steps.last == Cont(Var("b"), Mu(Var("a"), Var("x"))))
  }

  test("Evaluator - continuation mu inline roundtrip") {
    val expr = parse("[a](#a.[a]x)")
    val result = Evaluator.evalSteps(expr).last
    assert(result == parse("[a]x"))
  }
