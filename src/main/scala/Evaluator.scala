package lambdamu

import lambdamu.Expr
import scala.collection.mutable

object Evaluator:
  val env = mutable.Map.empty[String, Expr]

  def resolve(expr: Expr): Expr = expr match
    case Expr.Var(n) if env.contains(n) => resolve(env(n))
    case Expr.Lam(x, b)                 => Expr.Lam(x, resolve(b))
    case Expr.App(f, a)                 => Expr.App(resolve(f), resolve(a))
    case Expr.Freeze(a, e)              => Expr.Freeze(a, resolve(e))
    case Expr.Mu(a, u)                  => Expr.Mu(a, resolve(u))
    case other                          => other

  def subst(x: String, e: Expr, expr: Expr): Expr = expr match
    case Expr.Var(y)         => if y == x then e else expr
    case Expr.Lam(y, b)      => if y == x then expr else Expr.Lam(y, subst(x, e, b))
    case Expr.App(f, a)      => Expr.App(subst(x, e, f), subst(x, e, a))
    case Expr.Freeze(a, w)   => Expr.Freeze(a, subst(x, e, w))
    case Expr.Mu(a, u)       => Expr.Mu(a, subst(x, e, u))

  def rename(beta: String, alpha: String, expr: Expr): Expr = expr match
    case Expr.Lam(x, b)      => Expr.Lam(x, rename(beta, alpha, b))
    case Expr.App(f, a)      => Expr.App(rename(beta, alpha, f), rename(beta, alpha, a))
    case Expr.Freeze(g, w)   => Expr.Freeze(if g == beta then alpha else g, rename(beta, alpha, w))
    case Expr.Mu(g, u)       => if g == beta then expr else Expr.Mu(g, rename(beta, alpha, u))
    case _                   => expr

  def app(beta: String, v: Expr, expr: Expr): Expr = expr match
    case Expr.Lam(x, b)      => Expr.Lam(x, app(beta, v, b))
    case Expr.App(f, a)      => Expr.App(app(beta, v, f), app(beta, v, a))
    case Expr.Freeze(a, w)   => Expr.Freeze(a, if a == beta then Expr.App(app(beta, v, w), v) else app(beta, v, w))
    case Expr.Mu(a, u)       => if a == beta then expr else Expr.Mu(a, app(beta, v, u))
    case _                   => expr

  private def reduceInternal(expr: Expr): (Expr, Boolean) = expr match
    case Expr.Freeze(a1, Expr.Freeze(a2, e2)) if a1 == a2 => (Expr.Freeze(a1, e2), true)
    case Expr.App(Expr.Lam(x, u), v)                      => (subst(x, v, u), true)
    case Expr.App(Expr.Mu(b, u), v)                       => (Expr.Mu(b, app(b, v, u)), true)
    case Expr.Freeze(a, Expr.Mu(b, u))                    => (rename(b, a, u), true)
    case Expr.Mu(a, Expr.Freeze(b, e)) if a == b          => (e, true)
    case Expr.Lam(x, b)                                   => val (rb, c) = reduceInternal(b); (Expr.Lam(x, rb), c)
    case Expr.App(f, a)                                   =>
      val (rf, c1) = reduceInternal(f)
      val (ra, c2) = reduceInternal(a)
      (Expr.App(rf, ra), c1 || c2)
    case Expr.Freeze(a, w)                                => val (rw, c) = reduceInternal(w); (Expr.Freeze(a, rw), c)
    case Expr.Mu(a, u)                                    => val (ru, c) = reduceInternal(u); (Expr.Mu(a, ru), c)
    case _                                                => (expr, false)

  def evalSteps(expr: Expr): List[Expr] =
    val buf = mutable.ListBuffer(expr)
    var curr = expr
    var changed = true
    while changed do
      val (next, c) = reduceInternal(curr)
      if c then buf += next
      curr = next
      changed = c
    buf.toList

  def reduceOnce(expr: Expr): Expr = evalSteps(expr).lift(1).getOrElse(expr)
  def eval(expr: Expr): Expr = evalSteps(expr).last

