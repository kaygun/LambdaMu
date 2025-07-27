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
      if count >= maxSteps then
        println(s"Warning: Maximum steps ($maxSteps) reached, evaluation may be incomplete")
        current
      else
        val next = current.step
        if next.alphaEq(current) then current
        else loop(next, count + 1)
    loop(this, 0)

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

case class Lam(param: Var, body: Expr) extends Expr:
  def pretty = s"λ${param.name}.${body.pretty}"
  def prettyWithParens(needsParens: Boolean) = 
    if needsParens then s"(λ${param.name}.${body.pretty})" else pretty
  def freeVars = body.freeVars - param
  def freeContVars = body.freeContVars
  def alphaEq(that: Expr, env: Map[String, String]) = that match
    case Lam(otherParam, otherBody) =>
      val f = Expr.fresh()
      val renamedBody = body.subst(Map(param.name -> Var(f)), Map.empty)
      val renamedOtherBody = otherBody.subst(Map(otherParam.name -> Var(f)), Map.empty)
      renamedBody.alphaEq(renamedOtherBody, env)
    case _ => false
  def subst(t: Map[String, Expr], c: Map[String, Expr]) =
    if (t.values ++ c.values).flatMap(e => e.freeVars ++ e.freeContVars).exists(_ == param)
    then
      val f = Var(Expr.fresh(param.name))
      Lam(f, body.subst(Map(param.name -> f), Map()).subst(t - param.name, c))
    else Lam(param, body.subst(t - param.name, c))
  override def step = reduction.getOrElse {
    val r = body.step
    if !r.alphaEq(body) then Lam(param, r) else this
  }

case class Mu(param: Var, body: Expr) extends Expr:
  def pretty = s"μ${param.name}.${body.pretty}"
  def prettyWithParens(needsParens: Boolean) = 
    if needsParens then s"(μ${param.name}.${body.pretty})" else pretty
  def freeVars = body.freeVars - param
  def freeContVars = body.freeContVars - param
  def alphaEq(that: Expr, env: Map[String, String]) = that match
    case Mu(otherParam, otherBody) =>
      val f = Expr.fresh()
      val renamedBody = body.subst(Map(param.name -> Var(f)), Map.empty)
      val renamedOtherBody = otherBody.subst(Map(otherParam.name -> Var(f)), Map.empty)
      renamedBody.alphaEq(renamedOtherBody, env)
    case _ => false
  def subst(t: Map[String, Expr], c: Map[String, Expr]) =
    if (t.values ++ c.values).flatMap(e => e.freeVars ++ e.freeContVars).exists(_ == param)
    then
      val f = Var(Expr.fresh(param.name))
      Mu(f, body.subst(Map(param.name -> f), Map(param.name -> f)).subst(t, c - param.name))
    else Mu(param, body.subst(t, c - param.name))
  override def step = reduction.getOrElse {
    val r = body.step
    if !r.alphaEq(body) then Mu(param, r) else this
  }
  override def reduction = body match
    case Cont(Var(c), x) if param.name == c => Some(x)
    case _ => None

case class Appl(head: Expr, arg: Expr) extends Expr:
  def pretty = head match
    case _: Lam | _: Mu => s"(${head.pretty}) ${argWithParens}"
    case _ => s"${head.pretty} ${argWithParens}"
  
  private def argWithParens = arg match
    case _: Appl | _: Lam | _: Mu => s"(${arg.pretty})"
    case _ => arg.pretty
  def freeVars = head.freeVars ++ arg.freeVars
  def freeContVars = head match
    case Var(n) => Set(Var(n)) ++ arg.freeContVars
    case _ => head.freeContVars ++ arg.freeContVars
  def alphaEq(that: Expr, env: Map[String, String]) = that match
    case Appl(otherHead, otherArg) =>
      head.alphaEq(otherHead, env) && arg.alphaEq(otherArg, env)
    case _ => false
  def subst(t: Map[String, Expr], c: Map[String, Expr]) = Appl(head.subst(t, c), arg.subst(t, c))
  override def step = reduction.getOrElse {
    val nh = head.step
    val na = arg.step
    val r = Appl(nh, na)
    if !this.alphaEq(r) then r else this
  }
  override def reduction = head match
    case Lam(x, b) => Some(b.betaReduce(x, arg))
    case _ => None

case class Cont(head: Expr, arg: Expr) extends Expr:
  def pretty = s"[${head.pretty}] ${argWithParens}"
  
  private def argWithParens = arg match
    case _: Appl | _: Lam | _: Mu => s"(${arg.pretty})"
    case _ => arg.pretty
  def freeVars = head.freeVars ++ arg.freeVars
  def freeContVars = head match
    case Var(n) => Set(Var(n)) ++ arg.freeContVars
    case _ => head.freeContVars ++ arg.freeContVars
  def alphaEq(that: Expr, env: Map[String, String]) = that match
    case Cont(otherHead, otherArg) =>
      head.alphaEq(otherHead, env) && arg.alphaEq(otherArg, env)
    case _ => false
  def subst(t: Map[String, Expr], c: Map[String, Expr]) = head match
    case Var(n) if c.contains(n) => Cont(c(n).subst(t, c), arg.subst(t, c))
    case _ => Cont(head.subst(t, c), arg.subst(t, c))
  override def step = reduction.getOrElse {
    val nh = head.step
    val na = arg.step
    val r = Cont(nh, na)
    if !this.alphaEq(r) then r else this
  }
  override def reduction = (head, arg) match
    case (Var(a2), Mu(Var(a1), b)) if a1 == a2 => Some(b)  // [α](μα.M) → M
    case (Var(a2), Mu(Var(a1), b)) if a1 != a2 => Some(b.subst(Map.empty, Map(a1 -> Var(a2))))  // [α](μβ.M) → M[α/β]
    case (Var(a2), Cont(Var(a1), m)) if a1 == a2 => Some(Cont(Var(a1), m))  // [α][α]M → [α]M
    case _ => None
