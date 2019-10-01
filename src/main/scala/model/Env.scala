package model

import collection.mutable.{HashMap, ListBuffer, Stack}
import scala.collection.mutable

class Env(parentEnv: Option[Env], var allocated: Int = 0) {
  val ids = mutable.HashMap.empty[String, (Type, Int)]
  var maxAllocated: Int = allocated

  def openEnv() = new Env(Some(this), maxAllocated)

  def closeEnv(): Unit = {
    assert(parentEnv.nonEmpty)
    parentEnv.get.maxAllocated = maxAllocated
  }

  def registerIdentifier(identifier: Identifier, tp: Type): Unit = {
    if (ids.contains(identifier.name)) throw new Exception(f"${identifier.name} is used.")
    ids += identifier.name -> (tp, allocated)
    allocated += tp.getSize
    maxAllocated = maxAllocated.max(allocated)
  }

  def getIdentifier(identifier: Identifier): Option[(Type, Int)] = {
    ids.get(identifier.name).orElse(parentEnv match {
      case Some(env) => env.getIdentifier(identifier)
      case None => None
    })
  }
}