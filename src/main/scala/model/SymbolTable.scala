package model

import scala.collection.mutable

class SymbolTable {
  val ids = mutable.Stack.empty[mutable.HashMap[String, (Type, Int)]]
  var allocated = 0
  var maxAllocated = 0

  def openEnv(): Unit = ids.push(mutable.HashMap.empty)
  def closeEnv(): Unit = {
    allocated -= ids.last.values.map(_._1.getSize).sum
    ids.pop()
  }

  def registerIdentifier(identifier: Identifier, tp: Type): Unit = {
    if (ids.exists(_.contains(identifier.name))) throw new Exception(f"${identifier.name} is used.")
    ids.last += identifier.name -> (tp, allocated)
    allocated += tp.getSize
    maxAllocated = maxAllocated.max(allocated)
  }

  def getIdentifier(identifier: Identifier): Option[(Type, Int)] =
    ids.foldRight(None: Option[(Type, Int)]) {
      case (id, None) => id.get(identifier.name)
      case (_, r) => r
    }
}