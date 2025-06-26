package lambdamu

import lambdamu.Expr

object PrettyPrinter:
  def pretty(expr: Expr): String = pretty(expr, Map.empty, Set.empty)._1

  private def pretty(expr: Expr, env: Map[String, String], used: Set[String]): (String, Set[String]) = expr match
    case Expr.Var(n) => (env.getOrElse(n, n), used)
    case Expr.Lam(orig, body) =>
      val base = Iterator.from(0).map(i => if i == 0 then orig else s"$orig$i").dropWhile(used.contains).next()
      val (bs, u) = pretty(body, env + (orig -> base), used + base)
      (s"λ$base.$bs", u)
    case Expr.App(f, a) =>
      val (fs, u1) = pretty(f, env, used)
      val (as, u2) = pretty(a, env, u1)
      val left = f match
        case Expr.App(_, _) => s"($fs)"
        case _ => fs
      val right = a match
        case Expr.App(_, _) | Expr.Lam(_, _) | Expr.Mu(_, _) | Expr.Freeze(_, _) => s"($as)"
        case _ => as
      (s"$left $right", u2)
    case Expr.Freeze(alpha, e) =>
      val (es, u) = pretty(e, env, used)
      (s"[$alpha]$es", u)
    case Expr.Mu(alpha, e) =>
      val (es, u) = pretty(e, env, used)
      (s"μ$alpha.$es", u)

