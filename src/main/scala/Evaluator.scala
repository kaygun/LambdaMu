// === Evaluator ===
object Evaluator:
  def evalExpr(expr: Expr, maxSteps: Int = 1000): Expr =
    @annotation.tailrec
    def loop(current: Expr, count: Int): Expr =
      if count >= maxSteps then 
        println(s"Warning: Maximum steps ($maxSteps) reached, evaluation may be incomplete")
        current
      else
        val next = current.reduceOnce
        if next.alphaEq(current) then current
        else loop(next, count + 1)
    
    loop(expr, 0)
