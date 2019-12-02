import model.{CXInt, SingleIdentifier, SymbolTable}
import org.scalatest.FunSuite


class CXSpec extends FunSuite {
  def ok(code: String)(xs: List[Any]): Unit = {
    assert(Compiler.go(code) == xs.map(_.toString))
  }

  def ce(code: String): Unit = {
    intercept[Exception](Compiler.go(code))
  }

  test("symbol table") {
    val t = new SymbolTable
    val id = SingleIdentifier("x")
    t.openEnv()
    t.openEnv()
    t.registerIdentifier(id, CXInt())
    assert(t.getIdentifier(id) == Some(CXInt(), 0))
    t.closeEnv()
    assert(t.getIdentifier(id).isEmpty)
    t.closeEnv()
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

  test("variable usage must after declaration") {
    val code =
      """
        |{ write x; }
        |""".stripMargin
    ce(code)
  }

  test("declaration does not escape compound statement") {
    val code =
      """
        |{ { int x; } write x; }
        |""".stripMargin
    ce(code)
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

  test("declaration in for") {
    val code =
      """
        |{ for (int i = 0; i < 10; i = i + 1) write i; }
        |""".stripMargin
    ok(code)((0 until 10).toList)
  }

  test("declaration in for does not escape") {
    val code =
      """
        |{
        | for (int i = 0; i < 10; i = i + 1) write i;
        | write i;
        |}
        |""".stripMargin
    ce(code)
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

  test("simple do while") {
    val code =
      """
        |{
        | int i = 0;
        | do { write i; i = i + 1; } while (i < 10);
        |}
        |""".stripMargin
    ok(code)((0 until 10).toList)
  }

  test("function without parameter") {
    val code =
      """
        |int f() { return 1; }
        |{ write f(); }
        |""".stripMargin
    ok(code)(List(1))
  }

  test("simple break") {
    val code =
      """
        |{
        |   for (int i = 0; i < 10; i = i + 1) {
        |       write i;
        |       if (i == 5) break;
        |   }
        |}
        |""".stripMargin
    ok(code)((0 to 5).toList)
  }

  test("break in nested loop") {
    val code =
      """
        |{
        |   int s = 0;
        |   for (int i = 0; i < 10; i = i + 1)
        |     for (int j = 0; j < 10; j = j + 1) {
        |         if (j > i) break;
        |         s = s + j;
        |     }
        |   write s;
        |}
        |""".stripMargin
    ok(code)(List(165))
  }

  test("simple continue") {
    val code =
      """
        |{
        |   for (int i = 0; i < 10; i = i + 1) {
        |     if (i <= 5) continue;
        |     write i;
        |   }
        |}
        |""".stripMargin
    ok(code)((6 until 10).toList)
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

  test("declare array") {
    val code =
      """
        |{ int a[10]; }
        |""".stripMargin
    ok(code)(List())
  }

  test("array assignment and output") {
    val code =
      """
        |{ int a[10];  a[2] = 5; write a[2]; }
        |""".stripMargin
    ok(code)(List(5))
  }

  test("2d array assignment and output") {
    val code =
      """
        |{
        |   int a[10][5];
        |   for (int i = 0; i < 10; i = i + 1)
        |     for (int j = 0; j < 5; j = j + 1)
        |       { a[i][j] = i * 5 + j; write a[i][j]; }
        |}
        |""".stripMargin
    ok(code)((0 until 5 * 10).toList)
  }

  test("xor support") {
    val code =
      """
        |{ write true xor false; write true xor true; }
        |""".stripMargin
    ok(code)(List("t", "f"))
  }

  test("exit support") {
    val code =
      """
        |{ write 1; exit; write 2; }
        |""".stripMargin
    ok(code)(List(1))
  }

  test("negative number") {
    val code =
      """
        |{
        |   int x = 233;
        |   write -x;
        |}
        |""".stripMargin
    ok(code)(List(-233))
  }

  test("self prefix increment & decrement") {
    val code =
      """
        |{ int x = 1; write ++x; write x; int y = 1; write --y; write y; }
        |""".stripMargin
    ok(code)(List(2, 2, 0, 0))
  }

//  test("self postfix increment & decrement") {
//    val code =
//      """
//        |{ int x = 1; write x++; write x; }
//        |""".stripMargin
//    ok(code)(List(1, 0))
//  }
}
