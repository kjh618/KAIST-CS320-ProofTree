package prooftree

object Env {
  type Env = Map[String, Value]

  def envToString(env: Env): String = {
    if (env.isEmpty)
      "∅"
    else
      env.map({ case (n, v) => s"$n ↦ $v" }).mkString("[", ", ", "]")
  }

  def envDomainToString(env: Env): String = {
    if (env.isEmpty)
      "∅"
    else
      env.map({ case (n, _) => s"$n" }).mkString("[", ", ", "]")
  }

  def envToReducedString(env: Env, numNotReduce: Int): String = {
    if (env.isEmpty)
      "∅"
    else
      env.map({ case (n, v) => s"$n ↦ ${v.toReducedString(numNotReduce)}" }).mkString("[", ", ", "]")
  }
}