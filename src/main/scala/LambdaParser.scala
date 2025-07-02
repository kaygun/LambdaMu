// === Parser ===
import scala.util.parsing.combinator.JavaTokenParsers

object LambdaParser extends JavaTokenParsers:
  def variable: Parser[Var] = ident ^^ Var.apply
  def lambda: Parser[Expr] = ("\\" | "λ") ~> ident ~ "." ~ expr ^^ { case p ~ _ ~ b => Lam(Var(p), b) }
  def mu: Parser[Expr] = ("#" | "μ") ~> ident ~ "." ~ expr ^^ { case p ~ _ ~ b => Mu(Var(p), b) }
  def cont: Parser[Expr] = "[" ~> ident ~ "]" ~ atom ^^ { case c ~ _ ~ a => Cont(Var(c), a) }
  def atom: Parser[Expr] = lambda | mu | cont | variable | "(" ~> expr <~ ")"
  def expr: Parser[Expr] = rep1(atom) ^^ (_.reduceLeft(Appl.apply))

  def parseExpr(input: String): Either[String, Expr] =
    parseAll(expr, input) match
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(s"Parse error: $msg")

// === Substitution ===
object Substitution:
  def apply(bind: Var, repl: Expr, expr: Expr): Expr =
    val freeInRepl = repl.freeVars
    expr match
      case v: Var => if v == bind then repl else v
      case Appl(f, a) => Appl(apply(bind, repl, f), apply(bind, repl, a))
      case Cont(f, a) => Cont(apply(bind, repl, f), apply(bind, repl, a))
      case Lam(p, b) => 
        if p == bind then
          Lam(p, b)
        else if freeInRepl.contains(p) then
          val fresh = Var(s"${p.name}_${Gensym.fresh()}")
          Lam(fresh, apply(bind, repl, apply(p, fresh, b)))
        else Lam(p, apply(bind, repl, b))
      case Mu(p, b) => 
        if p == bind then
          Mu(p, b)
        else if freeInRepl.contains(p) then
          val fresh = Var(s"${p.name}_${Gensym.fresh()}")
          Mu(fresh, apply(bind, repl, apply(p, fresh, b)))
        else Mu(p, apply(bind, repl, b))
