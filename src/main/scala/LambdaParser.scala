package lambdamu

import lambdamu.Expr
import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers:
    def identifier: Parser[String] = "[a-zA-Z_]\\w*".r
    def lam: Parser[Expr]          = ("\\" | "λ") ~> identifier ~ ("." ~> expr) ^^ { case x ~ e => Expr.Lam(x, e) }
    def mu: Parser[Expr]           = ("mu" | "μ") ~> identifier ~ ("." ~> expr) ^^ { case a ~ e => Expr.Mu(a, e) }
    def freeze: Parser[Expr]       = ("[" ~> identifier <~ "]") ~ expr ^^ { case a ~ e => Expr.Freeze(a, e) }
    def variable: Parser[Expr]     = identifier ^^ Expr.Var.apply
    def term: Parser[Expr]         = lam | mu | freeze | variable | "(" ~> expr <~ ")"
    def app: Parser[Expr]          = term ~ rep(term) ^^ { case t ~ ts => ts.foldLeft(t)(Expr.App.apply)  }
    def expr: Parser[Expr]         = app
    def parse(input: String): Expr = parseAll(expr, input).getOrElse(throw new IllegalArgumentException("Parse error"))


