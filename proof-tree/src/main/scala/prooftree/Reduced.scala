package prooftree

import scala.collection.mutable

object Reduced {
  val reduceLength = 30

  val exprs: mutable.Map[Expr, String] = mutable.Map()
  val values: mutable.Map[Value, String] = mutable.Map()

  def initialize(): Unit = {
    exprs.clear()
    values.clear()
  }
}