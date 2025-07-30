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
    val result = e1.alphaEq(e2)
    if (!result) {
      println(s"Alpha equivalence FAILED!")
      println(s"Expression 1: ${e1.pretty}")
      println(s"Expression 2: ${e2.pretty}")
    }
    result shouldBe true
  }

  // Helper to get free term variables
  def freeTermVars(expr: Expr): Set[TermVar] = expr.freeNames.collect {
    case tv: TermVar => tv
  }

  // Helper to get free continuation variables  
  def freeContVars(expr: Expr): Set[ContVar] = expr.freeNames.collect {
    case cv: ContVar => cv
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
    result match {
      case Lam(param, body) => 
        param.name should not be "y"  // bound variable renamed
        body shouldBe parse("y")       // still refers to free y
      case _ => fail("Expected lambda expression")
    }
  }

  it should "demonstrate lazy evaluation - arguments not evaluated until needed" in {
    // In lazy evaluation: (λx.y) (infinite_loop) should return y without evaluating infinite_loop
    val expr = parse("(λx.y) ((λx.x x) (λx.x x))")  // (λx.y) applied to omega
    val result = expr.step  // Should step to y without evaluating omega
    result shouldBe parse("y")
  }

  "Mu reduction" should "perform mu reduction when continuation variable matches" in {
    val expr = parse("μα.[α] x")
    val result = expr.step
    result shouldBe parse("x")
  }

  it should "not perform reduction when continuation variable doesn't match" in {
    val expr = parse("μα.[β] x")
    val result = expr.step
    result shouldBe expr  // Should not reduce
  }

  "Structural reduction" should "apply continuation to mu expression with same variable" in {
    val expr = parse("[α] (μα.x)")
    val result = expr.step
    result shouldBe parse("x")
  }

  it should "apply continuation to mu expression with different variable" in {
    val expr = parse("[α] (μβ.x)")
    val result = expr.step
    result shouldBe parse("x")
  }

  it should "apply continuation with proper substitution" in {
    val expr = parse("[α] (μβ.[β] y)")
    val result = expr.step
    result shouldBe parse("[α] y")
  }

  "Free variables" should "identify free term variables correctly" in {
    freeTermVars(parse("x")) shouldBe Set(TermVar("x"))
    freeTermVars(parse("λx.x")) shouldBe Set.empty
    freeTermVars(parse("λx.y")) shouldBe Set(TermVar("y"))
    freeTermVars(parse("λx.x y")) shouldBe Set(TermVar("y"))
  }

  it should "identify free continuation variables correctly" in {
    freeContVars(parse("x")) shouldBe Set.empty
    freeContVars(parse("[α] x")) shouldBe Set(ContVar("α"))
    freeContVars(parse("μα.x")) shouldBe Set.empty
    freeContVars(parse("μα.[β] x")) shouldBe Set(ContVar("β"))
    freeContVars(parse("μα.[α] x")) shouldBe Set.empty
  }

  "Substitution" should "substitute free variables" in {
    val expr = parse("x")
    val result = expr.subst(Map(TermVar("x") -> parse("y")))
    result shouldBe parse("y")
  }

  it should "not substitute bound variables" in {
    val expr = parse("λx.x")
    val result = expr.subst(Map(TermVar("x") -> parse("y")))
    shouldBeAlphaEq(result, parse("λx.x"))
  }

  it should "avoid variable capture" in {
    val expr = parse("λy.x")
    val result = expr.subst(Map(TermVar("x") -> parse("y")))
    result match {
      case Lam(param, body) =>
        param.name should not be "y"  // bound variable renamed
        body shouldBe parse("y")       // body should be the substituted free y
      case _ => fail("Expected lambda expression")
    }
  }

  "Lazy evaluation vs Normalization" should "show the difference" in {
    // Church numeral arithmetic: succ 1 = 2
    val expr = parse("(λn.λf.λx.f (n f x)) (λf.λx.f x)")
    
    // Lazy evaluation stops at weak head normal form
    val lazy_result = expr.eval()
    lazy_result.pretty should include("λf")  // Should be a lambda but not fully reduced inside
    
    // Normalization continues reducing inside lambda bodies
    val normalized = lazy_result.normalize()
    val expected = parse("λf.λx.f (f x)")
    shouldBeAlphaEq(normalized, expected)
  }

  "Normalization" should "reduce inside lambda bodies" in {
    val expr = parse("λf.f ((λx.x) y)")
    val normalized = expr.normalize()
    normalized shouldBe parse("λf.f y")
  }

  it should "reduce inside application arguments" in {
    val expr = parse("f ((λx.x) y)")
    val normalized = expr.normalize()  
    normalized shouldBe parse("f y")
  }

  it should "handle nested reductions" in {
    val expr = parse("λf.λx.f ((λy.y) x)")
    val normalized = expr.normalize()
    normalized shouldBe parse("λf.λx.f x")
  }

  it should "work with mu expressions in reduction context" in {
    val expr = parse("μα.[α] x")  // This is in reduction context
    val normalized = expr.normalize()
    normalized shouldBe parse("x")  // This SHOULD reduce
  }

  it should "preserve mu expressions not in reduction context" in {
    val expr = parse("λf.f (μα.[β] x)")  // mu that CANNOT reduce (α ≠ β)
    val normalized = expr.normalize()
    normalized shouldBe parse("λf.f (μα.[β] x)")  // Should remain unchanged
  }

  it should "reduce structural congruence during normalization" in {
    val expr = parse("λf.[α] (μβ.[β] x)")  // [α] (μβ.[β] x) should reduce to [α] x
    val normalized = expr.normalize()
    normalized shouldBe parse("λf.[α] x")
  }

  it should "not change expressions already in normal form" in {
    val expr = parse("λf.λx.f x")
    val normalized = expr.normalize()
    normalized shouldBe expr
  }

  "Standard reduction rules" should "demonstrate all reduction types" in {
    // Beta: (λx.M) N → M[N/x]
    val beta = parse("(λx.x) y")
    beta.step shouldBe parse("y")

    // Mu: μα.[α] M → M  
    val mu = parse("μα.[α] x")
    mu.step shouldBe parse("x")

    // Structural: [α](μβ.c) → c[α/β]
    val structural = parse("[α] (μβ.[β] y)")
    structural.step shouldBe parse("[α] y")
  }

  "Classical logic features" should "demonstrate double negation elimination" in {
    val doubleNegElim = parse("λx.μα.x (λy.[α] y)")
    val doubleNegatedProof = parse("λf.f z")
    val application = Appl(doubleNegElim, doubleNegatedProof)
    val result = application.eval()
    result shouldBe parse("z")
  }

  it should "demonstrate Peirce's law with correct expectations" in {
    val peirce = parse("λf.μα.f (λx.[α] x)")
    val testFunction = parse("λg.μβ.[α] b")  // Note: α is FREE here
    val application = Appl(peirce, testFunction)
    val result = application.eval()
    
    // The result should be μα1.μβ.[α] b where:
    // - α1 is bound by outer μ (renamed to avoid capture)  
    // - β is bound by inner μ
    // - α is FREE (from the original testFunction)
    
    result match {
      case Mu(outerParam, Mu(innerParam, Cont(Var(ContVar(freeAlpha)), Var(TermVar("b"))))) =>
        // Check structure
        innerParam.name shouldBe "β"
        
        // The continuation variable should be the original free α, not the bound outer param
        freeAlpha should not be outerParam.name
        freeAlpha shouldBe "α"  // Original free variable
        
      case other => 
        fail(s"Expected μα1.μβ.[α] b structure, got: ${other.pretty}")
    }
  }

  "Complex expressions" should "handle function composition" in {
    val expr = parse("(λf.λg.λx.f (g x)) (λy.y) (λz.z)")
    val result = expr.eval().normalize()  // Need normalization for full reduction
    val expected = parse("λx.x")
    shouldBeAlphaEq(result, expected)
  }

  it should "handle mixed lambda-mu expressions" in {
    val expr = parse("(λx.μα.[α] x) (f y)")
    val result = expr.eval()
    result shouldBe parse("f y")
  }

  "Pretty printing" should "handle parentheses correctly" in {
    parse("(λx.x) y").pretty shouldBe "(λx.x) y"
    parse("f (g h)").pretty shouldBe "f (g h)"
    parse("[α] (λx.x)").pretty shouldBe "[α] (λx.x)"
    parse("λx.(x y)").pretty shouldBe "λx.x y"
  }

  it should "print mu expressions correctly" in {
    parse("μα.[α] x").pretty shouldBe "μα.[α] x"
    parse("(μα.x) y").pretty shouldBe "(μα.x) y"
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
    result.pretty should include("λx.x x")  // Should still be some form of the expression
  }

  it should "demonstrate lazy evaluation prevents infinite loops in unused arguments" in {
    // (λx.y) omega should terminate in lazy evaluation
    val omega = parse("(λx.x x) (λx.x x)")
    val expr = Appl(parse("λx.y"), omega)
    val result = expr.eval(maxSteps = 5)  // Should terminate quickly
    result shouldBe parse("y")
  }
}
