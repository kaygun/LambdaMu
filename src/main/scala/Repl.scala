package lambdamu

import lambdamu.{Expr, Evaluator, Parser, PrettyPrinter}

object Repl:
  def run(): Unit =
    println("λμ-Calculus REPL (Scala 3). Type :q to quit.")
    var continue = true

    def handleLine(raw: String): Unit =
      val line = raw.trim
      if line.startsWith("let ") && line.contains("=") then
        val parts = line.drop(4).split("=", 2).map(_.trim)
        if parts.length == 2 then
          val name = parts(0)
          val rhs = parts(1)
          try
            val expr = Parser.parse(rhs)
            Evaluator.env(name) = Evaluator.resolve(expr)
            println(s"let $name = ${PrettyPrinter.pretty(Evaluator.env(name))}")
          catch case ex: Exception => println(s"Definition error: ${ex.getMessage}")
        else println("Error: malformed let statement.")
      else if line.startsWith("load \"") && line.endsWith("\"") then
        val filename = line.stripPrefix("load \"").stripSuffix("\"")
        try
          val source = scala.io.Source.fromFile(filename)
          try source.getLines().filter(_.nonEmpty).foreach { ln =>
            println(s"> $ln")
            handleLine(ln.trim)
          } finally source.close()
        catch case ex: Exception => println(s"Could not load '$filename': ${ex.getMessage}")
      else if line.startsWith("save \"") && line.endsWith("\"") then
        val filename = line.stripPrefix("save \"").stripSuffix("\"")
        try
          val writer = new java.io.PrintWriter(filename)
          try
            for ((name, expr) <- Evaluator.env) do
              val prettyExpr = PrettyPrinter.pretty(expr)
              writer.println(s"let $name = $prettyExpr")
            println(s"Environment saved to '$filename'.")
          finally writer.close()
        catch case ex: Exception =>
          println(s"Could not save to '$filename': ${ex.getMessage}")
      else if line.nonEmpty then
        try
          val expr = Evaluator.resolve(Parser.parse(line))
          Evaluator.evalSteps(expr).zipWithIndex.foreach((e, i) => println(s"[$i] ${PrettyPrinter.pretty(e)}"))
        catch case ex: Exception => println(s"Error: ${ex.getMessage}")

    while continue do
      print("> "); Console.flush()
      scala.io.StdIn.readLine() match
        case null | ":q" => continue = false
        case input        => handleLine(input)
    println("Goodbye.")

