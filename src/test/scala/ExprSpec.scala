import model._
import org.scalatest.FlatSpec

class ExprSpec extends FlatSpec {
  behavior of "Expr parser"

  val parser = new CXParser

  it should "parse 1 + 2" in {
    parser.parseAll(parser.expression, "1 + 2") match {
      case parser.Success(r, _) => assert(r == BinaryOp("+", Num(1), Num(2)))
    }
  }

  it should "parse a = 1" in {
    parser.parseAll(parser.expression, "a = 1") match {
      case parser.Success(r, _) => assert(r == AssignExpr(SingleIdentifier("a"), Num(1)))
    }
  }
}
