import model.Scope

import scala.io.Source


object Compiler extends App {
  val inputFile = Source.fromFile("src/test/cx/expr.cx")
  val inputSource = inputFile.mkString

  val parser = new CXParser
  parser.parseAll(parser.statement, inputSource) match {
    case parser.Success(r, n) =>
      println(r)
      val code = (new CXCodeGenerator).gen_statement(r, new Scope(None))
      println(code)
      (new PCodeVM).interpreter(code)
    case parser.NoSuccess(err, next) =>
      println("failed to parse CX input " +
        "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
        err + "\n" +
        next.pos.longString)
  }
}
