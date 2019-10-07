package prooftree

case class ProofTree(premises: List[ProofTree], conclusion: String) {
  override def toString: String = {
    // TODO: Clean up the code
    def arrange(strs: List[String]): (String, Int) = {
      val widths = strs.map(_.split("\n").map(_.length).max)
      val maxHeight = strs.map(_.count(_ == '\n') + 1).max

      val resLines = strs.map(str => {
        val lines = str.split("\n")
        if (lines.length < maxHeight)
          List.fill(maxHeight - lines.length)("") ++ lines
        else
          List() ++ lines
      }).transpose

      val between = "  "
      val res = resLines.foldLeft("")((linesBefore, currentLine) => {
          linesBefore + "\n" +
            currentLine.foldLeft(("", 0))((tup, current) => {
              val (before, idx) = tup
              (before + s"%-${widths(idx)}s".format(current) + between, idx + 1)
            })._1
        })

      (res, widths.sum + widths.length * between.length)
    }

    if (premises == Nil)
      conclusion
    else {
      val (premisesStr, width) = arrange(premises.map(_.toString))
      premisesStr + "\n" +
        "―" * (width max conclusion.length) + "\n" +
        conclusion
    }
  }
}