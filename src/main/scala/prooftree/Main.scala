package prooftree

import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExportTopLevel

object Main {
  private val program = document.getElementById("programTextarea").asInstanceOf[dom.html.TextArea]
  private val proofTree = document.getElementById("proofTreeTextarea").asInstanceOf[dom.html.TextArea]

  @JSExportTopLevel("runProgram")
  def runProgram(): Unit = {
    proofTree.value = ProofTreeDrawer.run(program.value)
  }

  def main(args: Array[String]): Unit = {
  }
}