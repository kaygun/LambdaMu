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

abstract class Bind(val param: Var, val body: Expr) extends Expr:
  def withBody(p: Var, b: Expr): Bind
  def freeVars = body.freeVars - param
  def freeContVars = body.freeContVars
  def alphaEq(that: Expr, env: Map[String, String]) = that match
    case b: Bind if getClass == b.getClass =>
      val f = Expr.fresh()
      body.alphaEq(b.body, env + (param.name -> f, b.param.name -> f))
    case _ => false
  def subst(t: Map[String, Expr], c: Map[String, Expr]) =
    if (t.values ++ c.values).flatMap(e => e.freeVars ++ e.freeContVars).exists(_ == param)
    then
      val f = Var(s"${param.name}_${Expr.fresh()}")
      withBody(f, body.subst(Map(param.name -> f), Map()).subst(t - param.name, c))
    else withBody(param, body.subst(t - param.name, c))
  override def step = reduction.getOrElse {
    val r = body.step
    if !r.alphaEq(body) then withBody(param, r) else this
  }

abstract class Pair(val head: Expr, val arg: Expr) extends Expr:
  def withParts(h: Expr, a: Expr): Pair
  def freeVars = head.freeVars ++ arg.freeVars
  def freeContVars = head.freeContVars ++ arg.freeContVars
  def alphaEq(that: Expr, env: Map[String, String]) = that match
    case p: Pair if getClass == p.getClass =>
      head.alphaEq(p.head, env) && arg.alphaEq(p.arg, env)
    case _ => false
  def subst(t: Map[String, Expr], c: Map[String, Expr]) = withParts(head.subst(t, c), arg.subst(t, c))
  override def step = reduction.getOrElse {
    val nh = head.step
    val na = arg.step
    val r = withParts(nh, na)
    if !alphaEq(r) then r else this
  }

case class Var(name: String) extends Expr:
  def pretty = name
  def freeVars = Set(this)
  def freeContVars = Set.empty
  def alphaEq(that: Expr, env: Map[String, String]) = that match
    case Var(y) => env.getOrElse(name, name) == env.getOrElse(y, y)
    case _ => false
  def subst(t: Map[String, Expr], c: Map[String, Expr]) = t.getOrElse(name, c.getOrElse(name, this))

case class Lam(override val param: Var, override val body: Expr) extends Bind(param, body):
  def withBody(p: Var, b: Expr) = Lam(p, b)
  def pretty = s"λ${param.name}.${body.pretty}"

case class Mu(override val param: Var, override val body: Expr) extends Bind(param, body):
  def withBody(p: Var, b: Expr) = Mu(p, b)
  def pretty = s"μ${param.name}.${body.pretty}"
  override def freeContVars = body.freeContVars - param
  override def subst(t: Map[String, Expr], c: Map[String, Expr]) =
    if (t.values ++ c.values).flatMap(e => e.freeVars ++ e.freeContVars).exists(_ == param)
    then
      val f = Var(s"${param.name}_${Expr.fresh()}")
      Mu(f, body.subst(Map(param.name -> f), Map()).subst(t, c - param.name))
    else Mu(param, body.subst(t, c - param.name))
  override def reduction = body match
    case Cont(Var(c), x) if param.name == c && !x.freeContVars(param) => Some(x)
    case _ => None

case class Appl(override val head: Expr, override val arg: Expr) extends Pair(head, arg):
  def withParts(h: Expr, a: Expr) = Appl(h, a)
  def pretty = s"${head.pretty} ${arg.pretty}".trim
  override def reduction = head match
    case Lam(x, b) => Some(b.betaReduce(x, arg))
    case _ => None

case class Cont(override val head: Expr, override val arg: Expr) extends Pair(head, arg):
  def withParts(h: Expr, a: Expr) = Cont(h, a)
  def pretty = s"[${head.pretty}] ${arg.pretty}"
  override def freeContVars = head match
    case Var(n) => Set(Var(n)) ++ arg.freeContVars
    case _ => head.freeContVars ++ arg.freeContVars
  override def subst(t: Map[String, Expr], c: Map[String, Expr]) = head match
    case Var(n) if c.contains(n) => Cont(c(n).subst(t, c), arg.subst(t, c))
    case _ => Cont(head.subst(t, c), arg.subst(t, c))
  override def reduction = (head, arg) match
    case (Var(a2), Mu(Var(a1), b)) if a1 == a2 => Some(b)
    case (Var(a2), Cont(Var(a1), _)) if a1 == a2 => Some(arg)
    case _ => None
