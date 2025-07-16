// === AST ===
sealed trait Expr:
  def reduction: Option[Expr] = None
  def pretty: String

  def freeVars: Set[Var] = this match
    case v: Var  => Set(v)
    case b: Bind => b.body.freeVars - b.param
    case p: Pair => p.head.freeVars ++ p.arg.freeVars

  // Alpha equivalence comparison to determine structural equality modulo renaming 
  def alphaEq(that: Expr): Boolean =
    def loop(e1: Expr, e2: Expr, env: Map[String, String]): Boolean = (e1, e2) match
      case (Var(x), Var(y)) => env.getOrElse(x, x) == env.getOrElse(y, y)
      case (b1: Bind, b2: Bind) =>
        val fresh = Gensym.fresh()
        loop(b1.body, b2.body, env + (b1.param.name -> fresh, b2.param.name -> fresh))
      case (p1: Pair, p2: Pair) =>
        loop(p1.head, p2.head, env) && loop(p1.arg, p2.arg, env)
      case _ => false
    loop(this, that, Map.empty)

  def subst(termSubst: Map[String, Expr], contSubst: Map[String, Expr]): Expr
  def betaReduce(bind: Var, repl: Expr): Expr
  def reduceOnce: Expr =
    reduction match
      case Some(next) => next.reduceOnce
      case None => this

// === Abstract base classes ===
abstract class Pair(val head: Expr, val arg: Expr) extends Expr:
  def withParts(newHead: Expr, newArg: Expr): Pair

  override def subst(termSubst: Map[String, Expr], contSubst: Map[String, Expr]): Expr =
    withParts(head.subst(termSubst, contSubst), arg.subst(termSubst, contSubst))

  override def betaReduce(bind: Var, repl: Expr): Expr =
    withParts(head.betaReduce(bind, repl), arg.betaReduce(bind, repl))

  override def reduceOnce: Expr =
    super.reduceOnce match
      case res if res != this => res
      case _ =>
        val next = withParts(head.reduceOnce, arg.reduceOnce)
        if !this.alphaEq(next) then next else this
        //val h2 = head.reduceOnce
        //val a2 = arg.reduceOnce
        //if !h2.alphaEq(head) || !a2.alphaEq(arg) then
        //  withParts(h2, a2).reduceOnce else this

abstract class Bind(val param: Var, val body: Expr) extends Expr:
  def withBody(newParam: Var, newBody: Expr): Bind

  override def subst(termSubst: Map[String, Expr], contSubst: Map[String, Expr]): Expr =
    val freeInSubs = termSubst.values ++ contSubst.values
    if freeInSubs.flatMap(_.freeVars).exists(_ == param) then
      val fresh = Var(s"${param.name}_${Gensym.fresh()}")
      val renamedBody = body.subst(Map(param.name -> fresh), Map.empty)
      withBody(fresh, renamedBody.subst(termSubst - param.name, contSubst))
    else
      withBody(param, body.subst(termSubst - param.name, contSubst))

  override def betaReduce(bind: Var, repl: Expr): Expr =
    if param == bind then this
    else if repl.freeVars.contains(param) then
      val fresh = Var(s"${param.name}_${Gensym.fresh()}")
      val renamed = body.betaReduce(param, fresh)
      withBody(fresh, renamed.betaReduce(bind, repl))
    else
      withBody(param, body.betaReduce(bind, repl))

  override def reduceOnce: Expr =
    super.reduceOnce match
      case res if res != this => res
      case _ =>
        val b2 = body.reduceOnce
        if !b2.alphaEq(body) then withBody(param, b2).reduceOnce else this

// === Concrete nodes ===
case class Var(name: String) extends Expr:
  def pretty: String = name

  override def subst(termSubst: Map[String, Expr], contSubst: Map[String, Expr]): Expr =
    termSubst.getOrElse(name, this)

  override def betaReduce(bind: Var, repl: Expr): Expr =
    if this == bind then repl else this

case class Appl(override val head: Expr, override val arg: Expr) extends Pair(head, arg):
  override def withParts(newHead: Expr, newArg: Expr): Pair = Appl(newHead, newArg)

  def pretty: String = s"${head.pretty}" + (arg match
    case Var(_) => s" ${arg.pretty}"
    case _ => s" (${arg.pretty})")

  override def reduction: Option[Expr] = head match
    case Lam(x, body) => Some(body.betaReduce(x, arg)) // β-reduction
    case Mu(v, body)  => Some(body.subst(Map.empty, Map(v.name -> arg))) // continuation substitution
    case _            => None

case class Cont(override val head: Expr, override val arg: Expr) extends Pair(head, arg):
  override def withParts(newHead: Expr, newArg: Expr): Pair = Cont(newHead, newArg)

  def pretty: String = s"[${head.pretty}] ${arg.pretty}"

  override def reduction: Option[Expr] = (head, arg) match
    case (Mu(_, m), _)                            => Some(m)
    case (Var(a2), Mu(Var(a1), b)) if a1 == a2    => Some(b)
    case (Var(a2), Cont(Var(a1), _)) if a1 == a2  => Some(arg)
    case _                                        => None

case class Lam(override val param: Var, override val body: Expr) extends Bind(param, body):
  override def withBody(newParam: Var, newBody: Expr): Bind = Lam(newParam, newBody)

  def pretty: String = s"λ${param.name}.${body.pretty}"

  override def reduction: Option[Expr] =
    body.reduction.filterNot(_.alphaEq(body)).map(Lam(param, _))

case class Mu(override val param: Var, override val body: Expr) extends Bind(param, body):
  override def withBody(newParam: Var, newBody: Expr): Bind = Mu(newParam, newBody)

  def pretty: String = s"μ${param.name}.${body.pretty}"

  override def reduction: Option[Expr] = this match
    case Mu(Var(a), Cont(Var(c), x)) if a == c => Some(x)
    case _ => body.reduction.filterNot(_.alphaEq(body)).map(Mu(param, _))
