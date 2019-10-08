package prooftree

// Expression in CFWAE
trait Expr {
  def toReducedString: String
}

case class Num(num: Int) extends Expr {
  override def toString: String = num.toString

  override def toReducedString: String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None => num.toString
  }
}

case class Add(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"{+ $left $right}"

  override def toReducedString: String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None =>
      val leftStr = Reduced.exprs.getOrElse(left, left.toReducedString)
      val rightStr = Reduced.exprs.getOrElse(right, right.toReducedString)
      s"{+ $leftStr $rightStr}"
  }
}

case class Sub(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"{- $left $right}"

  override def toReducedString: String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None =>
      val leftStr = Reduced.exprs.getOrElse(left, left.toReducedString)
      val rightStr = Reduced.exprs.getOrElse(right, right.toReducedString)
      s"{- $leftStr $rightStr}"
  }
}

case class Mul(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"{* $left $right}"

  override def toReducedString: String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None =>
      val leftStr = Reduced.exprs.getOrElse(left, left.toReducedString)
      val rightStr = Reduced.exprs.getOrElse(right, right.toReducedString)
      s"{* $leftStr $rightStr}"
  }
}

case class With(name: String, value: Expr, body: Expr) extends Expr {
  override def toString: String = s"{with {$name $value} $body}"

  override def toReducedString: String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None =>
      val valueStr = Reduced.exprs.getOrElse(value, value.toReducedString)
      val bodyStr = Reduced.exprs.getOrElse(body, body.toReducedString)
      s"{with {$name $valueStr} $bodyStr}"
  }
}

case class Id(name: String) extends Expr {
  override def toString: String = name

  override def toReducedString: String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None => name
  }
}

case class Fun(param: String, body: Expr) extends Expr {
  override def toString: String = s"{fun {$param} $body}"

  override def toReducedString: String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None =>
      val bodyStr = Reduced.exprs.getOrElse(body, body.toReducedString)
      s"{fun {$param} $bodyStr}"
  }
}

case class App(fun: Expr, arg: Expr) extends Expr {
  override def toString: String = s"{$fun $arg}"

  override def toReducedString: String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None =>
      val funStr = Reduced.exprs.getOrElse(fun, fun.toReducedString)
      val argStr = Reduced.exprs.getOrElse(arg, arg.toReducedString)
      s"{$funStr $argStr}"
  }
}

case class If0(cond: Expr, tru: Expr, fls: Expr) extends Expr {
  override def toString: String = s"{if0 $cond $tru $fls}"

  override def toReducedString: String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None =>
      val condStr = Reduced.exprs.getOrElse(cond, cond.toReducedString)
      val truStr = Reduced.exprs.getOrElse(tru, tru.toReducedString)
      val flsStr = Reduced.exprs.getOrElse(fls, fls.toReducedString)
      s"{if0 $condStr $truStr $flsStr}"
  }
}