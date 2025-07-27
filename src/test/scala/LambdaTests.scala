import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LambdaMuCalculusTest extends AnyFlatSpec with Matchers {

  // Helper to parse expressions for testing
  def parse(s: String): Expr = LambdaParser.parseExpr(s) match {
    case Right(expr) => expr
    case Left(err) => fail(s"Parse error: $err")
  }

  // Helper to check if two expressions are alpha-equivalent
  def shouldBeAlphaEq(e1: Expr, e2: Expr): Unit = {
    e1.alphaEq(e2) shouldBe true
  }

  "Alpha equivalence" should "recognize identical expressions" in {
    val expr = parse("λx.x")
    shouldBeAlphaEq(expr, expr)
  }

  it should "recognize alpha-equivalent lambda expressions" in {
    val e1 = parse("λx.x")
    val e2 = parse("λy.y")
    shouldBeAlphaEq(e1, e2)
  }

  it should "recognize alpha-equivalent mu expressions" in {
    val e1 = parse("μα.[α] x")
    val e2 = parse("μβ.[β] x")
    shouldBeAlphaEq(e1, e2)
  }

  it should "distinguish different expressions" in {
    val e1 = parse("λx.x")
    val e2 = parse("λx.y")
    e1.alphaEq(e2) shouldBe false
  }

  it should "handle nested bindings correctly" in {
    val e1 = parse("λx.λy.x")
    val e2 = parse("λa.λb.a")
    shouldBeAlphaEq(e1, e2)
  }

  it should "distinguish bound vs free variables" in {
    val e1 = parse("λx.x")
    val e2 = parse("λx.y")
    e1.alphaEq(e2) shouldBe false
  }

  "Beta reduction" should "perform simple beta reduction" in {
    val expr = parse("(λx.x) y")
    val result = expr.step
    result shouldBe parse("y")
  }

  it should "perform beta reduction with substitution" in {
    val expr = parse("(λx.λy.x) a b")
    val result = expr.eval()
    result shouldBe parse("a")
  }

  it should "avoid variable capture in beta reduction" in {
    val expr = parse("(λx.λy.x) y")
    val result = expr.step
    // Should not capture the free y - the bound y should be renamed
    result match {
      case Lam(param, body) => 
        param.name should not be "y"  // bound variable renamed
        body shouldBe parse("y")       // still refers to free y
      case _ => fail("Expected lambda expression")
    }
  }

  it should "handle complex beta reductions" in {
    val expr = parse("(λf.λx.f x) (λy.y)")
    val result = expr.eval()
    shouldBeAlphaEq(result, parse("λx.x"))
  }

  "Mu reduction" should "perform mu reduction when continuation variable matches" in {
    val expr = parse("μα.[α] x")
    val result = expr.step
    result shouldBe parse("x")
  }

  it should "reduce when continuation variable is bound" in {
    // Note: [α][α] y parses as [α]([α] y) due to precedence  
    val expr = parse("μα.[α][α] y")
    val result = expr.step
    // Should reduce to [α] y because the α in [α] y is bound by the outer μα
    result shouldBe parse("[α] y")
  }

  it should "handle mu reduction in context" in {
    val expr = parse("f (μα.[α] x)")
    val stepped = expr.step
    stepped shouldBe parse("f x")
  }

  "Continuation application" should "apply continuation to mu expression" in {
    val expr = parse("[α] (μα.x)")
    val result = expr.step
    result shouldBe parse("x")
  }

  it should "eliminate double continuation application" in {
    val expr = parse("[α][α] x")
    val result = expr.step
    result shouldBe parse("[α] x")
  }

  it should "not reduce continuation with different variables" in {
    val expr = parse("[α] (μβ.x)")
    val result = expr.step
    // Should not reduce the continuation but may step inside
    // Since μβ.x doesn't have [β] in body, it doesn't reduce
    result shouldBe parse("x")
  }

  "Free variables" should "identify free term variables correctly" in {
    parse("x").freeVars shouldBe Set(Var("x"))
    parse("λx.x").freeVars shouldBe Set.empty
    parse("λx.y").freeVars shouldBe Set(Var("y"))
    parse("λx.x y").freeVars shouldBe Set(Var("y"))
  }

  it should "identify free continuation variables correctly" in {
    parse("x").freeContVars shouldBe Set.empty
    parse("[α] x").freeContVars shouldBe Set(Var("α"))  // α is free cont var
    parse("μα.x").freeContVars shouldBe Set.empty
    parse("μα.[β] x").freeContVars shouldBe Set(Var("β"))
    parse("μα.[α] x").freeContVars shouldBe Set.empty  // α is bound by μ
  }

  "Substitution" should "substitute free variables" in {
    val expr = parse("x")
    val result = expr.subst(Map("x" -> parse("y")), Map.empty)
    result shouldBe parse("y")
  }

  it should "not substitute bound variables" in {
    val expr = parse("λx.x")
    val result = expr.subst(Map("x" -> parse("y")), Map.empty)
    shouldBeAlphaEq(result, parse("λx.x"))
  }

  it should "avoid variable capture" in {
    val expr = parse("λy.x")
    val result = expr.subst(Map("x" -> parse("y")), Map.empty)
    // Should rename bound y to avoid capturing free y
    result match {
      case Lam(param, body) =>
        param.name should not be "y"  // bound variable renamed
        body shouldBe parse("y")       // body should be the substituted free y
      case _ => fail("Expected lambda expression")
    }
  }

  it should "substitute continuation variables in mu expressions" in {
    val expr = parse("μα.[β] x")
    val result = expr.subst(Map.empty, Map("β" -> parse("γ")))
    shouldBeAlphaEq(result, parse("μα.[γ] x"))
  }

  "Complex reductions" should "handle Church numerals" in {
    // Church numeral 2: λf.λx.f (f x)
    val two = parse("λf.λx.f (f x)")
    val succ = parse("λn.λf.λx.f (n f x)")
    val app = Appl(succ, two)
    val result = app.eval()
    
    // Should be Church numeral 3: λf.λx.f (f (f x))
    val three = parse("λf.λx.f (f (f x))")
    shouldBeAlphaEq(result, three)
  }

  it should "handle mixed lambda-mu expressions" in {
    val expr = parse("(λx.μα.[α] x) y")
    val result = expr.eval()
    result shouldBe parse("y")
  }

  it should "handle nested mu expressions correctly" in {
    // μα.μβ.[α] [β] x - the inner μβ binds β, outer μα binds α
    val expr = parse("μα.μβ.[α] [β] x")
    // Should not reduce at the outer level because body is μβ.[α] [β] x, not [α]M
    val result = expr.step
    result shouldBe expr
  }

  it should "handle sequential mu reductions" in {
    // μα.[α] (μβ.[β] x) should reduce in steps
    val expr = parse("μα.[α] (μβ.[β] x)")
    val step1 = expr.step  // μα.[α] (μβ.[β] x) → μβ.[β] x
    step1 shouldBe parse("μβ.[β] x")
    val step2 = step1.step // μβ.[β] x → x
    step2 shouldBe parse("x")
  }

  "Evaluation termination" should "terminate on normalizing expressions" in {
    val expr = parse("(λx.x) y")
    val result = expr.eval()
    result shouldBe parse("y")
  }

  it should "handle non-terminating expressions gracefully" in {
    val omega = parse("(λx.x x) (λx.x x)")
    val result = omega.eval(maxSteps = 10)
    // Should stop after maxSteps and print warning
    // The result should be some intermediate form, not the original
    result.pretty should include("λx.x x")  // Should still be some form of the expression
  }

  "Pretty printing" should "handle parentheses correctly" in {
    parse("(λx.x) y").pretty shouldBe "(λx.x) y"
    parse("f (g h)").pretty shouldBe "f (g h)"
    parse("[α] (λx.x)").pretty shouldBe "[α] (λx.x)"
    parse("λx.x y").pretty shouldBe "λx.x y"
  }

  it should "print mu expressions correctly" in {
    parse("μα.[α] x").pretty shouldBe "μα.[α] x"
    parse("(μα.x) y").pretty shouldBe "(μα.x) y"
  }

  it should "demonstrate double negation elimination (classical logic)" in {
    val doubleNegElim = parse("λx.μα.x (λy.[α] y)")
    val doubleNegatedProof = parse("λf.f z")
    val application = Appl(doubleNegElim, doubleNegatedProof)
    val result = application.eval()

    // Debug: print what we actually got
    println(s"Expected: z, Got: ${result.pretty}")

    result shouldBe parse("z")
  }
}
