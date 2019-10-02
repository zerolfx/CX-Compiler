import model._
import org.scalatest.{FlatSpec, FunSuite}


class CXSpec extends FunSuite {
  def ok(code: String)(xs: List[Any]): Unit = {
    assert(Compiler.go(code) == xs.map(_.toString))
  }

  test("simple arithmetic operation") {
    val code =
      """
        |{
        | write (100 + 2) * 3 % 233 / 1;
        |}
        |""".stripMargin
    ok(code)(List(73))
  }

  test("gcd") {
    val code =
      """
        |int gcd(int x, int y) {
        |    if (y == 0) return x;
        |    return gcd(y, x % y);
        |}
        |{ write gcd(192, 42); }
        |""".stripMargin
    ok(code)(List(6))
  }

  test("factorial") {
    val code =
      """
        |int fac(int x) {
        |    if (x == 0) return 1;
        |    return x * fac(x - 1);
        |}
        |{ write fac(10); }
        |""".stripMargin
    ok(code)(List(3628800))
  }

  test("simple if") {
    val code =
      """
        |{
        |const int x = 10;
        |if (x == 10) write 1; else write 2;
        |if (x != 10) write 1; else write 2;
        |}
        |""".stripMargin
    ok(code)(List(1, 2))
  }

  test("naive assignment chains") {
    val code =
      """
        |{
        |int x = 0, y = 0, z = 0;
        |x = y = z = 2;
        |write x + y + z;
        |}
        |""".stripMargin
    ok(code)(List(6))
  }
}
