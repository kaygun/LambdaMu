package lambdamu

enum Expr:
  case Var(name: String)
  case App(fun: Expr, arg: Expr)
  case Lam(param: String, body: Expr)
  case Freeze(alpha: String, expr: Expr)
  case Mu(alpha: String, expr: Expr)

