// === Gensym Utility ===
object Gensym:
  private var counter = 0
  def fresh(prefix: String = "_"): String =
    counter += 1
    s"$prefix$counter"

// === AST ===
sealed trait Expr:
  def freeVars: Set[Var] = this match
    case v: Var => Set(v)
    case Appl(f, a) => f.freeVars ++ a.freeVars
    case Cont(f, a) => f.freeVars ++ a.freeVars
    case Lam(p, b) => b.freeVars - p
    case Mu(p, b) => b.freeVars - p

  def alphaEq(that: Expr): Boolean =
    def loop(e1: Expr, e2: Expr, env: Map[String, String]): Boolean = (e1, e2) match
      case (Var(x), Var(y)) => env.getOrElse(x, x) == env.getOrElse(y, y)
      case (Appl(f1, a1), Appl(f2, a2)) => loop(f1, f2, env) && loop(a1, a2, env)
      case (Cont(f1, a1), Cont(f2, a2)) => loop(f1, f2, env) && loop(a1, a2, env)
      case (Lam(x, b1), Lam(y, b2)) =>
        val name = Gensym.fresh()
        loop(b1, b2, env + (x.name -> name, y.name -> name))
      case (Mu(x, b1), Mu(y, b2)) =>
        val name = Gensym.fresh()
        loop(b1, b2, env + (x.name -> name, y.name -> name))
      case _ => false
    loop(this, that, Map.empty)

case class Var(name: String) extends Expr
case class Lam(param: Var, body: Expr) extends Expr
case class Mu(param: Var, body: Expr) extends Expr
case class Appl(fun: Expr, arg: Expr) extends Expr
case class Cont(cont: Expr, arg: Expr) extends Expr

