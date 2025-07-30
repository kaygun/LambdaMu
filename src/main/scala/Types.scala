sealed trait Name {
  def name: String
}
case class TermVar(name: String) extends Name
case class ContVar(name: String) extends Name

sealed trait Expr:
  def pretty: String
  def freeNames: Set[Name]
  def alphaEq(that: Expr, env: Map[Name, Name] = Map.empty): Boolean
  def subst(substMap: Map[Name, Expr]): Expr
  def step: Expr = reduction.getOrElse(this)
  def reduction: Option[Expr] = None
  def eval(maxSteps: Int = 1000): Expr =
    @annotation.tailrec
    def loop(current: Expr, count: Int): Expr =
      if count >= maxSteps then current
      else
        val next = current.step
        if next.alphaEq(current) then current
        else loop(next, count + 1)
    loop(this, 0)

  def normalize(): Expr = this

object Expr:
  private var counter = 0
  def fresh(prefix: String = "_"): String =
    counter += 1
    s"$prefix$counter"

// Abstract base class for binding constructs (Lambda and Mu)
abstract class Binding(val param: Name, val body: Expr) extends Expr:
  def freeNames = body.freeNames - param
  def alphaEq(that: Expr, env: Map[Name, Name]) = that match
    case other: Binding if other.getClass == this.getClass =>
      val newEnv = env + (param -> other.param)
      body.alphaEq(other.body, newEnv)
    case _ => false
  def subst(substMap: Map[Name, Expr]) =
    if substMap.values.exists(_.freeNames.contains(param)) then
      val fresh = freshParam()
      reconstructWithFresh(fresh, body.subst(Map(param -> Var(fresh))).subst(substMap - param))
    else reconstruct(param, body.subst(substMap - param))
  override def step = reduction.getOrElse {
    body.step match
      case b if !b.alphaEq(body) => reconstruct(param, b)
      case _ => this
  }
  override def normalize() =
    val normBody = body.normalize()
    val candidate = reconstruct(param, normBody)
    val stepped = candidate.step
    if !stepped.alphaEq(candidate) then stepped.normalize() else candidate
    
  protected def freshParam(): Name
  protected def reconstruct(param: Name, body: Expr): Binding
  protected def reconstructWithFresh(fresh: Name, body: Expr): Binding

// Abstract base class for application constructs (Appl and Cont)
abstract class Application(val head: Expr, val arg: Expr) extends Expr:
  def freeNames = head.freeNames ++ arg.freeNames
  def alphaEq(that: Expr, env: Map[Name, Name]) = that match
    case other: Application if other.getClass == this.getClass =>
      head.alphaEq(other.head, env) && arg.alphaEq(other.arg, env)
    case _ => false
  def subst(substMap: Map[Name, Expr]) = reconstruct(head.subst(substMap), arg.subst(substMap))
  override def normalize() =
    val normH = head.normalize()
    val normA = arg.normalize()
    val candidate = reconstruct(normH, normA)
    val stepped = candidate.step
    if !stepped.alphaEq(candidate) then stepped.normalize() else candidate
    
  protected def reconstruct(head: Expr, arg: Expr): Application

case class Var(n: Name) extends Expr:
  def pretty = n.name
  def freeNames = Set(n)
  def alphaEq(that: Expr, env: Map[Name, Name]) = that match
    case Var(m) => env.getOrElse(n, n) == env.getOrElse(m, m)
    case _ => false
  def subst(substMap: Map[Name, Expr]) = substMap.getOrElse(n, this)

case class Lam(override val param: TermVar, override val body: Expr) extends Binding(param, body):
  def pretty = s"\u03bb${param.name}.${body.pretty}"
  protected def freshParam() = TermVar(Expr.fresh(param.name))
  protected def reconstruct(param: Name, body: Expr) = Lam(param.asInstanceOf[TermVar], body)
  protected def reconstructWithFresh(fresh: Name, body: Expr) = Lam(fresh.asInstanceOf[TermVar], body)

case class Mu(override val param: ContVar, override val body: Expr) extends Binding(param, body):
  def pretty = s"\u03bc${param.name}.${body.pretty}"
  protected def freshParam() = ContVar(Expr.fresh(param.name))
  protected def reconstruct(param: Name, body: Expr) = Mu(param.asInstanceOf[ContVar], body)
  protected def reconstructWithFresh(fresh: Name, body: Expr) = Mu(fresh.asInstanceOf[ContVar], body)
  override def reduction = body match
    case Cont(Var(ContVar(c)), x) if param.name == c => Some(x)
    case _ => None

case class Appl(override val head: Expr, override val arg: Expr) extends Application(head, arg):
  def pretty = head match
    case _: Lam | _: Mu => s"(${head.pretty}) ${argPretty}"
    case _ => s"${head.pretty} ${argPretty}"
  
  private def argPretty = arg match
    case _: Appl => s"(${arg.pretty})"  // Parenthesize nested applications 
    case _ => arg.pretty
    
  protected def reconstruct(head: Expr, arg: Expr) = Appl(head, arg)
  override def step = head match
    case Lam(x, b) => b.subst(Map(x -> arg))  // Beta reduction
    case _ =>
      val newHead = head.step
      if !newHead.alphaEq(head) then Appl(newHead, arg) else this

case class Cont(override val head: Expr, override val arg: Expr) extends Application(head, arg):
  def pretty = arg match
    case _: Lam | _: Mu => s"[${head.pretty}] (${arg.pretty})"
    case _ => s"[${head.pretty}] ${arg.pretty}"
  protected def reconstruct(head: Expr, arg: Expr) = Cont(head, arg)
  override def reduction = (head, arg) match
    case (Var(ContVar(a2)), Mu(ContVar(a1), b)) =>
      if a1 == a2 then Some(b)
      else Some(b.subst(Map(ContVar(a1) -> Var(ContVar(a2)))))
    case _ => None
  override def step = reduction.getOrElse {
    val newHead = head.step
    if !newHead.alphaEq(head) then Cont(newHead, arg) else this
  }
