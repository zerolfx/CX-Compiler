package model


object Ins {
  class T
  class N extends T
  object i extends N { override def toString: String = "i"}
  object r extends N { override def toString: String = "r"}
  object a extends T { override def toString: String = "a"}
  object b extends T { override def toString: String = "b"}
  object c extends T { override def toString: String = "c"}
  object v extends T { override def toString: String = "?"}

  // TODO change T to N
  def hlt = "hlt\n"
  def pop = "pop\n"
  def add(t: T) = f"add $t\n"
  def sub(t: T) = f"sub $t\n"
  def mul(t: T) = f"mul $t\n"
  def mod = "mod\n"
  def div(t: T) = f"div $t\n"
  def neg(t: T) = f"neg $t\n"
  def and = "and\n"
  def or = "or\n"
  def xor = "xor\n"
  def not = "not\n"
  def equ(t: T) = f"equ $t\n"
  def geq(t: T) = f"geq $t\n"
  def leq(t: T) = f"leq $t\n"
  def les(t: T) = f"les $t\n"
  def grt(t: T) = f"grt $t\n"
  def neq(t: T) = f"neq $t\n"
  def ldo(t: T, q: Int) = f"ldo $t $q\n"
  def ldc(t: T, q: String) = f"ldc $t $q\n"
  def ind(t: T) = f"ind $t\n"
  def sro(t: T, q: Int) = f"sro $t $q\n"
  def sto(t: T) = f"sto $t\n"
  def ujp(label: String) = f"ujp $label\n"
  def fjp(label: String) = f"fjp $label\n"
  def lod(t: T, p: Int, q: Int) = f"lod $t $p $q\n"
  def lda(p: Int, q: Int) = f"lda $p $q\n"
  def str(t: T, p: Int, q: Int) = f"str $t $p $q\n"

  def mst(p: Int) = f"mst $p\n"
  def cup(p: Int, s: String) = f"cup $p $s\n"
  def ssp(p: Int) = f"ssp $p\n"

  def retf = "retf\n"
  def retp = "retp\n"

  def dpl(t: T) = f"dpl $t\n"

  def out(t: T) = f"out $t\n"
  def in(t: T) = {
    assert(t != a)
    f"in $t\n"
  }
  def conv(from: T, to: T) = f"conv $from $to\n"

  private var labelCnt = 0
  def createLabel: String = {
    labelCnt += 1
    "l" + labelCnt
  }
  def label(s: String): String = s + ":\n"
}
