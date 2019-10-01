package model

case class Program(stmts: CompoundStmt) extends Node {
  override def gen(implicit env: Env): String = {
    val res = stmts.gen
    Ins.ssp(env.maxAllocated) + res
  }
}