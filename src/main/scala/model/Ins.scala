package model


object Ins {
  class T
  class N extends T
  object i extends N { override def toString: String = "i"}
  object r extends N { override def toString: String = "r"}
  object a extends T { override def toString: String = "a"}
  object b extends T { override def toString: String = "b"}
  object c extends T { override def toString: String = "c"}

  // TODO change T to N
  def add(t: T) = f"add $t\n"
  def sub(t: T) = f"sub $t\n"
  def mul(t: T) = f"mul $t\n"
  def div(t: T) = f"div $t\n"
  def neg(t: T) = f"div $t\n"
  def and = "and"
  def or = "or"
  def not = "not"
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
  def f(label: String) = f"f $label\n"
  def out(t: T) = f"out $t\n"
  def in(t: T) = {
    assert(t != a)
    f"in $t \n"
  }
}
