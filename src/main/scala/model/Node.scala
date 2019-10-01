package model

abstract class Node {
  def gen(implicit env: Env): String
}
