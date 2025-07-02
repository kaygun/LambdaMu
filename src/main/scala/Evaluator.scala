// === Evaluator ===
object Evaluator:
  def reduceExpr(expr: Expr): Expr = expr match
    case Appl(f, a) =>
      reduceHead(f, a).map(reduceExpr).getOrElse {
        val (f2, a2) = (reduceExpr(f), reduceExpr(a))
        if !f2.alphaEq(f) || !a2.alphaEq(a) then reduceExpr(Appl(f2, a2)) else expr
      }
    case Cont(f, a) =>
      reduceCont(f, a).map(reduceExpr).getOrElse {
        val (f2, a2) = (reduceExpr(f), reduceExpr(a))
        if !f2.alphaEq(f) || !a2.alphaEq(a) then reduceExpr(Cont(f2, a2)) else expr
      }
    case Lam(p, b) =>
      val b2 = reduceExpr(b)
      if !b2.alphaEq(b) then Lam(p, b2) else expr
    case Mu(Var(a), Cont(Var(c), x)) if a == c => reduceExpr(x)
    case Mu(p, b) =>
      val b2 = reduceExpr(b)
      if !b2.alphaEq(b) then Mu(p, b2) else expr
    case _ => expr

  private def reduceHead(f: Expr, a: Expr): Option[Expr] = f match
    case Lam(x, body) => Some(Substitution(x, a, body))
    case Mu(v, body) =>
      def subst(m: Expr): Expr = m match
        case Cont(Var(b), x) if v.name == b => Appl(a, subst(x))
        case Cont(Var(b), x) => Cont(Var(b), subst(x))
        case Cont(c, x) => Cont(subst(c), subst(x))
        case Appl(f, x) => Appl(subst(f), subst(x))
        case Lam(p, b) => Lam(p, subst(b))
        case Mu(p, b) => Mu(p, subst(b))
        case e => e
      Some(subst(body))
    case _ => None

  private def reduceCont(f: Expr, a: Expr): Option[Expr] = (f, a) match
    case (Mu(_, m), _) => Some(m)
    case (Var(a2), Mu(Var(a1), b)) if a1 == a2 => Some(b)
    case (Var(a2), Cont(Var(a1), x)) if a1 == a2 => Some(Cont(Var(a1), x))
    case _ => None

  def evalSteps(expr: Expr, maxSteps: Int = 1000): List[Expr] =
    @annotation.tailrec
    def loop(current: Expr, history: List[Expr], count: Int): List[Expr] =
      if count >= maxSteps then history :+ current
      else
        val next = reduceExpr(current)
        if history.exists(_.alphaEq(next)) then history :+ current
        else if next.alphaEq(current) then history :+ current
        else loop(next, history :+ current, count + 1)
    loop(expr, Nil, 0)
