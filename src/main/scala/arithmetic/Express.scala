package arithmetic

/**
  * Created by jeroendijkmeijer on 29/08/16.
  */

import org.scalajs.dom
import org.scalajs.dom.document

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

@JSExport
object Express extends JSApp {
  @JSExport
  def main(): Unit = {
    val f = new ExprFormatter
    val e1 = BinOp("*", BinOp("/", Number(1), Number(2)),
      BinOp("+", Var("x"), Number(1)))
    val e2 = BinOp("+", BinOp("/", Var("x"), Number(2)),
      BinOp("/", Number(1.5), Var("x")))
    val e3 = BinOp("/", e1, e2)
    def show(e: Expr) = println(f.format(e) + "\n\n")
    def showDerive(e: Expr, to: String) =
      show(f.derive(e, to))
    for (e <- Array(e1, e2, e3)) show(e)
    for (e <- Array(e1, e2, e3)) showDerive(e, "x")
  }

  @JSExport
  def appendPre(e: Expr, target: dom.Node, text: String): Unit = {
    val preNode = document.createElement("pre")
    val f = new ExprFormatter
    val textNode = document.createTextNode(f.format(e) + "\n\n")
    preNode.appendChild(textNode)
    target.appendChild(preNode)
  }
}
