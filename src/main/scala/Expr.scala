package lambdamu

enum Expr:
  case Var(name: String)
  case Lam(param: String, body: Expr)
  case App(fun: Expr, arg: Expr)
  case Freeze(alpha: String, expr: Expr)
  case Mu(alpha: String, expr: Expr)

