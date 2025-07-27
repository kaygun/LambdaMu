// === Parser ===
import scala.util.parsing.combinator.JavaTokenParsers

object LambdaParser extends JavaTokenParsers:
  def variable: Parser[Var] = ident ^^ Var.apply
  def lambda: Parser[Lam] = ("\\" | "λ") ~> ident ~ "." ~ expr ^^ { case p ~ _ ~ b => Lam(Var(p), b) }
  def mu: Parser[Mu] = ("#" | "μ") ~> ident ~ "." ~ expr ^^ { case p ~ _ ~ b => Mu(Var(p), b) }
  def cont: Parser[Cont] = "[" ~> ident ~ "]" ~ atom ^^ { case c ~ _ ~ a => Cont(Var(c), a) }
  def atom: Parser[Expr] = lambda | mu | cont | variable | "(" ~> expr <~ ")"
  def expr: Parser[Expr] = rep1(atom) ^^ (_.reduceLeft(Appl.apply))

  def parseExpr(input: String): Either[String, Expr] =
    parseAll(expr, input) match
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(s"Parse error: $msg")
