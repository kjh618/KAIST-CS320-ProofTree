package prooftree

object Env {
  type Env = Map[String, Value]

  def envToString(env: Env): String = env.map({ case (n, v) => s"$n ↦ $v" }).mkString("[", ", ", "]")

  def envDomainToString(env: Env): String = env.map({ case (n, _) => s"$n" }).mkString("[", ", ", "]")

  def envToReducedString(env: Env, numNotReduce: Int): String =
    env.map({ case (n, v) => s"$n ↦ ${v.toReducedString(numNotReduce)}" }).mkString("[", ", ", "]")
}