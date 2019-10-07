package prooftree

import scala.sys.error
import prooftree.PL._

object ProofTreeDrawer {
  def interp(expr: CFWAE, env: Env): (Value, ProofTree) = {
    type BinOp = (Int, Int) => Int

    def applyBinOp(op: BinOp, left: (Value, ProofTree), right: (Value, ProofTree)): (Value, ProofTree) = {
      val (leftRes, leftTree) = left
      val (rightRes, rightTree) = right
      (leftRes, rightRes) match {
        case (NumVal(n1), NumVal(n2)) =>
          val res = NumVal(op(n1, n2))
          (res, ProofTree(List(leftTree, rightTree), s"${envToString(env)} ⊢ $expr ⇒ $res"))
        case (v1, v2) =>
          error(s"not both numbers: $v1, $v2")
      }
    }

    expr match {
      case Num(n) =>
        val res = NumVal(n)
        (res, ProofTree(s"${envToString(env)} ⊢ $expr ⇒ $res"))

      case Add(l, r) =>
        applyBinOp(_ + _, interp(l, env), interp(r, env))
      case Sub(l, r) =>
        applyBinOp(_ - _, interp(l, env), interp(r, env))
      case Mul(l, r) =>
        applyBinOp(_ * _, interp(l, env), interp(r, env))

      case With(name, value, body) =>
        val (valueRes, valueTree) = interp(value, env)
        val (bodyRes, bodyTree) = interp(body, env + (name -> valueRes))
        (bodyRes, ProofTree(List(valueTree, bodyTree), s"${envToString(env)} ⊢ $expr ⇒ $bodyRes"))

      case Id(name) =>
        val res = env.getOrElse(name, error(s"free identifier: $name"))
        (res, ProofTree(s"$name ∈ ${envToString(env)}", s"${envToString(env)} ⊢ $expr ⇒ $res"))

      case Fun(param, body) =>
        val res = FunVal(param, body, env)
        (res, ProofTree(s"${envToString(env)} ⊢ $expr ⇒ $res"))

      case App(fun, arg) =>
        val (funRes, funTree) = interp(fun, env)
        funRes match {
          case FunVal(param, body, funEnv) =>
            val (argRes, argTree) = interp(arg, env)
            val (bodyRes, bodyTree) = interp(body, funEnv + (param -> argRes))
            (bodyRes, ProofTree(List(funTree, argTree, bodyTree), s"${envToString(env)} ⊢ $expr ⇒ $bodyRes"))
          case v => error(s"not a function: $v")
        }

      case If0(cond, tru, fls) =>
        val (condRes, condTree) = interp(cond, env)
        condRes match {
          case NumVal(0) =>
            val (truRes, truTree) = interp(tru, env)
            (truRes, ProofTree(List(condTree, truTree), s"${envToString(env)} ⊢ $expr ⇒ $truRes"))
          case v =>
            val (flsRes, flsTree) = interp(fls, env)
            (flsRes, ProofTree(List(condTree, ProofTree(s"$v ≠ 0"), flsTree), s"${envToString(env)} ⊢ $expr ⇒ $flsRes"))
        }
    }
  }

  def run(str: String): String = interp(PLParser(str), Map())._2.toString
}
