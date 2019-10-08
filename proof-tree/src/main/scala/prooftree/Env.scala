package prooftree

object Env {
  type Env = Map[String, Value]

  def envToString(env: Env): String = env.map({ case (n, v) => s"$n ↦ $v" }).mkString("[", ", ", "]")

  def envToReducedString(env: Env): String =
    env.map({ case (n, v) => s"$n ↦ ${v.toReducedString}" }).mkString("[", ", ", "]")
}