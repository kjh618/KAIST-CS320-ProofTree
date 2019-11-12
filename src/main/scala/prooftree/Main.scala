package prooftree

import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExportTopLevel

object Main {
  private val programInput = document.getElementById("programInput").asInstanceOf[dom.html.TextArea]
  private val proofTree = document.getElementById("proofTree").asInstanceOf[dom.html.TextArea]

  @JSExportTopLevel("runProgram")
  def runProgram(): Unit = {
    proofTree.value = try ProofTreeDrawer.run(programInput.value) catch {
      case e: RuntimeException => e.getMessage
      case e: Throwable => s"unknown error: $e"
    }
  }

  def main(args: Array[String]): Unit = {
  }
}