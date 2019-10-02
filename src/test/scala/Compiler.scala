import model.{Env, FunTable, SymbolTable}

import scala.io.Source


object Compiler extends App {
  def go(sourceCode: String): List[String] = {
    val parser = new CXParser
    parser.parseAll(parser.program, sourceCode) match {
      case parser.Success(r, n) =>
        println(r)
        val code = r.gen + "hlt\n"
        //      println(code)
        val lines: Array[String] = code.split("\n")
        lines.zipWithIndex.foreach { case (s, i) => println(f"$i: $s") }
        reflect.io.File("t.p").writeAll(code)
        sys.process.Process("./src/test/binary/Pmachine t.p").!!.linesIterator.toList.dropRight(3)
      case parser.NoSuccess(err, next) =>
        println("failed to parse CX input " +
          "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
          err + "\n" +
          next.pos.longString)
        throw new Exception("failed.")
    }
  }

  val inputFile = Source.fromFile("src/test/cx/expr.cx")
  val inputSource = inputFile.mkString
  println(go(inputSource))
}
