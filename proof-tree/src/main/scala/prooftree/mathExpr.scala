package prooftree

import prooftree.Env._

trait MathExpr {
  def toReducedString(): String
}

case class Implication(env: Env, expr: Expr, value: Value) extends MathExpr {
  override def toString: String = s"${envToString(env)} ⊢ $expr ⇒ $value"

  override def toReducedString(): String = {
    var exprStr = expr.toReducedString
    if (exprStr.length > Reduced.reduceLength) {
      Reduced.exprs += (expr -> f"e${Reduced.exprs.size}%01d")
      exprStr = expr.toReducedString
    }
    var valueStr = value.toReducedString
    if (valueStr.length > Reduced.reduceLength) {
      Reduced.values += (value -> f"v${Reduced.values.size}%01d")
      valueStr = value.toReducedString
    }
    val envStr = envToReducedString(env)
    s"$envStr ⊢ $exprStr ⇒ $valueStr"
  }
}

case class Membership(name: String, env: Env) extends MathExpr {
  override def toString: String = s"$name ∈ ${envToString(env)}"

  override def toReducedString(): String = {
    val envStr = envToReducedString(env)
    s"$name ∈ $envStr"
  }
}

case class Other(str: String) extends MathExpr {
  override def toString: String = str

  override def toReducedString(): String = str
}