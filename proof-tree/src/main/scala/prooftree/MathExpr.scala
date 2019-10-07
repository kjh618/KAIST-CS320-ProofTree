package prooftree

import PL._

trait MathExpr
case class Implication(env: Env, expr: CFWAE, value: Value) extends MathExpr {
  override def toString: String = s"${envToString(env)} ⊢ $expr ⇒ $value"
}
case class Other(str: String) extends MathExpr {
  override def toString: String = str
}