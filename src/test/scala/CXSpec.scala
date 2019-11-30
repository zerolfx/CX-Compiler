import org.scalatest.FunSuite


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

  test("simple for") {
    val code =
      """
        |{
        |int i = 1, s = 0;
        |for (; i < 10; i = i + 1) s = s + i;
        |write s;
        |}
        |""".stripMargin
    ok(code)(List(45))
  }

  test("simple while") {
    val code =
      """
        |{
        |int i = 0, s = 0;
        |while (i < 10) {
        |s = s + i;
        |i = i + 1;
        |}
        |write s;
        |}
        |""".stripMargin
    ok(code)(List(45))
  }

  test("function without parameter") {
    val code =
      """
        |int f() { return 1; }
        |{ write f(); }
        |""".stripMargin
    ok(code)(List(1))
  }

  test("function call function") {
    val code =
      """
        |int f() { return 1; }
        |int g() { return f(); }
        |{ write g(); }
        |""".stripMargin
    ok(code)(List(1))
  }

  test("default value in declaration") {
    val code =
      """
        |{ int x; write x; }
        |""".stripMargin
    ok(code)(List(0))
  }

  test("cast float to int") {
    val code =
      """
        |{ write (int)1.1; }
        |""".stripMargin
    ok(code)(List(1))
  }

  test("bool declaration and output") {
    val code =
      """
        |{ bool x; write x; }
        |""".stripMargin
    ok(code)(List("f"))
  }

  test("bool assignment") {
    val code =
      """
        |{ bool x = true; write x; }
        |""".stripMargin
    ok(code)(List("t"))
  }

  test("assign compare result to bool") {
    val code =
      """
        |{ bool x = 1 < 2; write x; }
        |""".stripMargin
    ok(code)(List("t"))
  }
}
