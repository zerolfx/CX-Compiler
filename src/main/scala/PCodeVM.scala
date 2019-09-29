import PCodeVM.BoolToInt

object PCodeVM {
  implicit class BoolToInt(val b:Boolean) extends AnyVal {
    def toInt: Int = if (b) 1 else 0
  }
}

final class PCodeVM {
  def parse_code(code: String): (String, Int, Int) =
    code.split(Array(',', ' ', '\t')).filterNot(_.isEmpty) match {
      case Array(i, a, b) => (i.toLowerCase, a.toInt, b.toInt)
    }

  @annotation.tailrec
  def _base(l: Int, b: Int, s: Array[Int]): Int =
    if (l == 0) b else _base(l - 1, s(b), s)

  def from_text(code: String): Unit =
    interpreter(code.split('\n').filterNot(_.isEmpty).map(parse_code).toList)

  def interpreter(code: List[(String, Int, Int)]): Unit = {
    var (t, b, p) = (0, 1, 0)
    val s = Array.ofDim[Int](20)
    val base = _base(_, b, s)

    def binary_op(f: (Int, Int) => Int): Unit = {
      t -= 1
      s(t) = f(s(t), s(t + 1))
    }

    s(1) = 0; s(2) = 0; s(3) = 0
    do {
      val i = code(p)
      println(s.toList)
      println(t, b, p, i)
      p += 1
      i match {
        case ("wri", 0, 0) =>
          println(s(t))
        case ("rdi", 0, 0) =>
          t += 1
          s(t) = io.StdIn.readInt()
        case ("lit", 0, a) =>
          t += 1
          s(t) = a
        case ("lod", l, a) =>
          t += 1
          s(t) = s(base(l) + a)
        case ("sto", l, a) =>
          s(base(l) + a) = s(t)
          t -= 1
        case ("cal", l, a) =>
          s(t + 1) = base(l)
          s(t + 2) = b
          s(t + 3) = p
          b = t + 1
          p = a
        case ("int", 0, a) =>
          t = t + a
        case ("jmp", 0, a) =>
          p = a
        case ("jpc", 0, a) =>
          if (s(t) == 0) p = a
          t -= 1
        case ("opr", 0, a) => a match {
          case 0 =>
            t = b - 1
            p = s(t + 3)
            b = s(t + 2)
          case 1 =>
            s(t) = -s(t)
          case 2 => binary_op(_ + _)
          case 3 => binary_op(_ - _)
          case 4 => binary_op(_ * _)
          case 5 => binary_op(_ / _)
          case 6 => s(t) &= 1
          case 8 => binary_op((a, b) => (a == b).toInt)
          case 9 => binary_op((a, b) => (a != b).toInt)
          case 10 => binary_op((a, b) => (a < b).toInt)
          case 11 => binary_op((a, b) => (a >= b).toInt)
          case 12 => binary_op((a, b) => (a > b).toInt)
          case 13 => binary_op((a, b) => (a <= b).toInt)
        }
      }
    } while (p != 0)
  }
}


