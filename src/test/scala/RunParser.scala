import scala.io.Source


object RunParser extends App {
  val inputFile = Source.fromFile("src/test/cx/expr.cx")
  val inputSource = inputFile.mkString

  val parser = new CXParser
  parser.parseAll(parser.statement, inputSource) match {
    case parser.Success(r, n) =>
      println(r)
    case parser.NoSuccess(err, next) =>
      println("failed to parse CX input " +
        "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
        err + "\n" +
        next.pos.longString)
  }
}