# A Lambda-Mu Interpreter in Scala3

This is an interpreter for [Lambda-Mu calculus][1] implemented in Scala3. I did use an LLM. The code I have is based on [a similar interpreter implemented in Haskell][2]. Type "sbt run" to start the interpreter. You may load a library using the command `load "filename"` inside the interpreter. You may also define short-cuts via `let <name> = <lambda-mu expression>`.

[1]: https://en.wikipedia.org/wiki/Lambda-mu_calculus
[2]: https://stackoverflow.com/questions/28752112/interpret-parigots-lambda-mu-calculus-in-haskell
