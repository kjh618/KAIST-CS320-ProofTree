package prooftree

import prooftree.Env._

trait MathExpr {
  def toReducedString(): String
}

case class Implication(env: Env, expr: Expr, value: Value) extends MathExpr {
  override def toString: String = s"${envToString(env)} ⊢ $expr ⇒ $value"

  override def toReducedString(): String = {
    val exprStr = expr.toReducedString(Reduced.numNotReduce)
    val valueStr = value.toReducedString(Reduced.numNotReduce)
    val envStr = envToReducedString(env, Reduced.numNotReduce)
    s"$envStr ⊢ $exprStr ⇒ $valueStr"
  }
}

case class Membership(name: String, env: Env) extends MathExpr {
  override def toString: String = s"$name ∈ ${envToString(env)}"

  override def toReducedString(): String = {
    val envStr = envToReducedString(env, Reduced.numNotReduce)
    s"$name ∈ $envStr"
  }
}

case class Other(str: String) extends MathExpr {
  override def toString: String = str

  override def toReducedString(): String = str
}