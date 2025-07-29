sealed trait Expr:
  def pretty: String
  def freeVars: Set[Var]
  def freeContVars: Set[Var]
  def alphaEq(that: Expr, env: Map[String, String] = Map.empty): Boolean
  def subst(termSubst: Map[String, Expr] = Map.empty, contSubst: Map[String, Expr] = Map.empty): Expr
  def betaReduce(bind: Var, repl: Expr): Expr = subst(Map(bind.name -> repl), Map.empty)
  def step: Expr = reduction.getOrElse(this)
  def reduction: Option[Expr] = None
  def eval(maxSteps: Int = 1000): Expr =
    @annotation.tailrec
    def loop(current: Expr, count: Int): Expr =
      if count >= maxSteps then current
      else
        val next = current.step
        if next.alphaEq(current) then current  // Stop at weak head normal form
        else loop(next, count + 1)
    loop(this, 0)  

  // Default normalize implementation (for Var)
  def normalize(): Expr = this

// Common trait for expressions with two sub-expressions (Appl and Cont)
trait BinaryExpr extends Expr:
  def head: Expr
  def arg: Expr
  
  def freeVars = head.freeVars ++ arg.freeVars
  
  def alphaEq(that: Expr, env: Map[String, String]) = that match
    case other: BinaryExpr if this.getClass == other.getClass =>
      head.alphaEq(other.head, env) && arg.alphaEq(other.arg, env)
    case _ => false
    
  override def step = reduction.getOrElse {
    val nh = head.step
    if !nh.alphaEq(head) then reconstruct(nh, arg) else this
  }
  
  override def normalize(): Expr = {
    // Both the head and the body are normalized
    val normalizedHead = head.normalize()
    val normalizedArg = arg.normalize()

    val withNormalized = reconstruct(normalizedHead, normalizedArg)

    val stepped = withNormalized.step
    if !stepped.alphaEq(withNormalized) then
      stepped.normalize()
    else
      withNormalized
  }
  
  protected def reconstruct(newHead: Expr, newArg: Expr): BinaryExpr

// Common trait for expressions with parameter binding (Lam and Mu)
trait BindingExpr extends Expr:
  def param: Var
  def body: Expr
  
  def freeVars = body.freeVars - param
  
  def alphaEq(that: Expr, env: Map[String, String]) = that match
    case other: BindingExpr if this.getClass == other.getClass =>
      val newEnv = env + (param.name -> other.param.name)
      body.alphaEq(other.body, newEnv)
    case _ => false
    
  protected def needsCapture(t: Map[String, Expr], c: Map[String, Expr]): Boolean =
    (t.values ++ c.values).flatMap(e => e.freeVars ++ e.freeContVars).exists(_ == param)
    
  override def step = reduction.getOrElse {
    val r = body.step
    if !r.alphaEq(body) then reconstruct(param, r) else this
  }
  
  override def normalize(): Expr = {
    // Only the body is normalized
    val normalizedBody = body.normalize()
    
    val withNormalized = reconstruct(param, normalizedBody)
    
    val stepped = withNormalized.step
    if !stepped.alphaEq(withNormalized) then
      stepped.normalize()
    else
      withNormalized
  }
  
  protected def reconstruct(param: Var, newBody: Expr): BindingExpr

object Expr:
  private var counter = 0
  def fresh(prefix: String = "_"): String =
    counter += 1
    s"$prefix$counter"

case class Var(name: String) extends Expr:
  def pretty = name
  def freeVars = Set(this)
  def freeContVars = Set.empty
  def alphaEq(that: Expr, env: Map[String, String]) = that match
    case Var(y) => env.getOrElse(name, name) == env.getOrElse(y, y)
    case _ => false
  def subst(t: Map[String, Expr], c: Map[String, Expr]) = t.getOrElse(name, c.getOrElse(name, this))

case class Lam(param: Var, body: Expr) extends BindingExpr:
  def pretty = s"λ${param.name}.${body.pretty}"
  def freeContVars = body.freeContVars
  
  def subst(t: Map[String, Expr], c: Map[String, Expr]) =
    if needsCapture(t, c) then
      val f = Var(Expr.fresh(param.name))
      Lam(f, body.subst(Map(param.name -> f), Map.empty).subst(t - param.name, c))
    else Lam(param, body.subst(t - param.name, c))
      
  protected def reconstruct(param: Var, newBody: Expr): BindingExpr = Lam(param, newBody)

case class Mu(param: Var, body: Expr) extends BindingExpr:
  def pretty = s"μ${param.name}.${body.pretty}"
  def freeContVars = body.freeContVars - param
  
  def subst(t: Map[String, Expr], c: Map[String, Expr]) =
    if needsCapture(t, c) then
      val f = Var(Expr.fresh(param.name))
      Mu(f, body.subst(Map(param.name -> f), Map(param.name -> f)).subst(t, c - param.name))
    else Mu(param, body.subst(t, c - param.name))
      
  protected def reconstruct(param: Var, newBody: Expr): BindingExpr = Mu(param, newBody)
  
  override def reduction = body match
    case Cont(Var(c), x) if param.name == c => Some(x)  // μα.[α]M → M (main mu reduction)
    case _ => None

case class Appl(head: Expr, arg: Expr) extends BinaryExpr:
  def pretty = {
    val headStr = head match {
      case _: Lam | _: Mu => s"(${head.pretty})"
      case _ => head.pretty
    }
    val argStr = arg match {
      case _: Appl | _: Lam | _: Mu => s"(${arg.pretty})"
      case _ => arg.pretty
    }
    s"$headStr $argStr"
  }
  
  def freeContVars = head match
    case Var(n) => Set(Var(n)) ++ arg.freeContVars
    case _ => head.freeContVars ++ arg.freeContVars
    
  def subst(t: Map[String, Expr], c: Map[String, Expr]) = Appl(head.subst(t, c), arg.subst(t, c))
  
  protected def reconstruct(newHead: Expr, newArg: Expr): BinaryExpr = Appl(newHead, newArg)
  
  override def reduction = head match
    case Lam(x, b) => Some(b.betaReduce(x, arg))
    case _ => None

case class Cont(head: Expr, arg: Expr) extends BinaryExpr:
  def pretty = {
    val argStr = arg match {
      case _: Appl | _: Lam | _: Mu => s"(${arg.pretty})"
      case _ => arg.pretty
    }
    s"[${head.pretty}] $argStr"
  }
  
  def freeContVars = head match
    case Var(n) => Set(Var(n)) ++ arg.freeContVars
    case _ => head.freeContVars ++ arg.freeContVars
    
  def subst(t: Map[String, Expr], c: Map[String, Expr]) = head match
    case Var(n) if c.contains(n) => Cont(c(n).subst(t, c), arg.subst(t, c))
    case _ => Cont(head.subst(t, c), arg.subst(t, c))
    
  protected def reconstruct(newHead: Expr, newArg: Expr): BinaryExpr = Cont(newHead, newArg)
  
  override def reduction = (head, arg) match
    case (Var(a2), Mu(Var(a1), b)) => 
      if a1 == a2 then Some(b)  // [α](μα.c) → c
      else Some(b.subst(Map.empty, Map(a1 -> Var(a2))))  // [α](μβ.c) → c[α/β] (structural reduction)
    case _ => None
