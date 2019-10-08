package prooftree

case class ProofTree(premises: List[ProofTree], conclusion: MathExpr) {
  def toParagraph: Paragraph = {
    if (premises.isEmpty) {
      Paragraph(conclusion.toString)
    }
    else {
      val premisesParagraph = Paragraph.arrange(premises.map(_.toParagraph))
      val conclusionStr = conclusion.toString
      val bars = "―" * (premisesParagraph.width max (conclusionStr.length + 4))
      Paragraph(premisesParagraph.lines :+ bars :+ ("  " + conclusionStr + "  "))
    }
  }

  override def toString: String = toParagraph.toString

  private def toReducedParagraphHelper(): Paragraph = {
    if (premises.isEmpty) {
      Paragraph(conclusion.toReducedString())
    }
    else {
      val premisesParagraph = Paragraph.arrange(premises.map(_.toReducedParagraphHelper()))
      val conclusionStr = conclusion.toReducedString()
      val bars = "―" * (premisesParagraph.width max (conclusionStr.length + 4))
      Paragraph(premisesParagraph.lines :+ bars :+ ("  " + conclusionStr + "  "))
    }
  }

  def toReducedParagraph(): Paragraph = {
    Paragraph(toReducedParagraphHelper().lines ++
      Reduced.exprs.map({ case (e, rs) =>
        s"* %s = %-${Reduced.notReduceLength * 2}s = %s".format(rs, e.toReducedString(Reduced.numNotReduce), e)
      }).toList.sorted ++
      Reduced.values.map({ case (v, rs) =>
        s"* %s = %-${Reduced.notReduceLength * 2}s = %s".format(rs, v.toReducedString(Reduced.numNotReduce), v)
      }).toList.sorted)
  }

  def toReducedString(): String = toReducedParagraph().toString
}

object ProofTree {
  def apply(mathExpr: MathExpr): ProofTree = ProofTree(Nil, mathExpr)

  def apply(premise: MathExpr, conclusion: MathExpr): ProofTree = ProofTree(List(ProofTree(premise)), conclusion)
}