import model.Env

import scala.io.Source


object Compiler extends App {
  val inputFile = Source.fromFile("src/test/cx/expr.cx")
  val inputSource = inputFile.mkString

  val parser = new CXParser
  parser.parseAll(parser.program, inputSource) match {
    case parser.Success(r, n) =>
      println(r)
      val code = r.gen(new Env(None)) + "hlt\n"
      println(code)
      reflect.io.File("t.p").writeAll(code)
      sys.process.Process("./src/test/binary/Pmachine t.p").!
    case parser.NoSuccess(err, next) =>
      println("failed to parse CX input " +
        "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
        err + "\n" +
        next.pos.longString)
  }
}
