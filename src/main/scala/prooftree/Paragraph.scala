package prooftree

case class Paragraph(lines: List[String]) {
  def width: Int = lines.map(_.length).max

  def height: Int = lines.length

  def normalized(height: Int): Paragraph = Paragraph(List.fill(height - this.height)("") ++ lines)

  override def toString: String = lines.mkString("\n")
}

object Paragraph {
  def apply(line: String): Paragraph = Paragraph(List(line))

  def arrange(paragraphs: List[Paragraph]): Paragraph = {
    val maxHeight = paragraphs.map(_.height).max
    val resLines = paragraphs.map(_.normalized(maxHeight).lines).transpose
    val widths = paragraphs.map(_.width).toVector

    Paragraph(resLines.map(_.foldLeft(("  ", 0))((tup, cur) => {
      val (acc, idx) = tup
      (acc + s"%-${widths(idx)}s  ".format(cur), idx + 1)
    })._1))
  }
}