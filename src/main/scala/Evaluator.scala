// === Evaluator ===
object Evaluator:
  private def betaReduction(bind: Var, repl: Expr, expr: Expr): Expr =
    val freeInRepl = repl.freeVars
    expr match
      case v: Var => if v == bind then repl else v
      case Appl(f, a) => Appl(betaReduction(bind, repl, f), betaReduction(bind, repl, a))
      case Cont(f, a) => Cont(betaReduction(bind, repl, f), betaReduction(bind, repl, a))
      case Lam(p, b) => 
        if p == bind then
          Lam(p, b)
        else if freeInRepl.contains(p) then
          val fresh = Var(s"${p.name}_${Gensym.fresh()}")
          Lam(fresh, betaReduction(bind, repl, betaReduction(p, fresh, b)))
        else Lam(p, betaReduction(bind, repl, b))
      case Mu(p, b) => 
        if p == bind then
          Mu(p, b)
        else if freeInRepl.contains(p) then
          val fresh = Var(s"${p.name}_${Gensym.fresh()}")
          Mu(fresh, betaReduction(bind, repl, betaReduction(p, fresh, b)))
        else Mu(p, betaReduction(bind, repl, b))

  private def reduceAppl(f: Expr, a: Expr): Option[Expr] = f match
    case Lam(x, body) => Some(betaReduction(x, a, body))
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

  def reduceExpr(expr: Expr): Expr = expr match
    case Appl(f, a) =>
      reduceAppl(f, a).map(reduceExpr).getOrElse {
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

  def evalExpr(expr: Expr, maxSteps: Int = 1000): Expr =
    @annotation.tailrec
    def loop(current: Expr, count: Int): Expr =
      if count >= maxSteps then current
      else
        val next = reduceExpr(current)
        if next.alphaEq(current) then current
        else loop(next, count + 1)
    loop(expr, 0)
