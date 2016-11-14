package arithmetic

/**
  * Created by jeroendijkmeijer on 29/08/16.
  */
import arithmetic.Element.elem

import scala.annotation.meta.field
import scala.scalajs.js.annotation.{JSExport, JSExportDescendentObjects}
@JSExportDescendentObjects
sealed abstract class Expr
@JSExport
case class Var(@(JSExport @field) name: String) extends Expr
@JSExport
case class Number(@(JSExport @field) num: Double) extends Expr
@JSExport
case class UnOp(@(JSExport @field)operator: String
                ,@(JSExport @field) arg: Expr) extends Expr
@JSExport
case class BinOp(@(JSExport @field) operator: String,
                 @(JSExport @field)left: Expr, @(JSExport @field) right: Expr) extends Expr
@JSExport
class ExprFormatter {
  // Contains operators in groups of increasing precedence
  private val opGroups =
    Array(
      Set("|", "||"),
      Set("&", "&&"),
      Set("ˆ"),
      Set("==", "!="),
      Set("<", "<=", ">", ">="),
      Set("+", "-"),
      Set("*", "%")
    )
  // A mapping from operators to their precedence
  private val precedence = {
    val assocs =
      for {
        i <- 0 until opGroups.length
        op <- opGroups(i)
      } yield op -> i
    assocs.toMap
  }
  private val unaryPrecedence = opGroups.length
  private val fractionPrecedence = -1
  // continued in Listing 15.21...


    private def format(e: Expr, enclPrec: Int): Element =
      e match {
        case Var(name) =>
          elem(name)
        case Number(num) =>
          def stripDot(s: String) =
            if (s endsWith ".0") s.substring(0, s.length - 2)
            else s
          elem(stripDot(num.toString))
        case UnOp(op, arg) =>
          elem(op) beside format(arg, unaryPrecedence)
        case BinOp("/", left, right) =>
          val top = format(left, fractionPrecedence)
          val bot = format(right, fractionPrecedence)
          val line = elem('-', top.width max bot.width, 1)
          val frac = top above line above bot
          if (enclPrec != fractionPrecedence) frac
          else elem(" ") beside frac beside elem(" ")
        case BinOp(op, left, right) =>
          val opPrec = precedence(op)
          val l = format(left, opPrec)
          val r = format(right, opPrec + 1)
          val oper = l beside elem(" " + op + " ") beside r
          if (enclPrec <= opPrec) oper
          else elem("(") beside oper beside elem(")")
      }
    @JSExport
    def format(e: Expr): Element = format(e, 0)

    @JSExport
    def derive(e: Expr, to: String): Expr = {
      val newExpr = e match {
        case BinOp("*", left, Var(t)) => BinOp("*", Var(t), left)
        case BinOp("+", left, Var(t)) => BinOp("*", Var(t), left)
        case _ => e
      }
      e match {
        case Var(to) => Number(1)
        case Var(n) => Number(0)
        case Number(n) => Number(0)
        case BinOp("*", Number(n1), Number(n2)) => Number(0)
        case BinOp("*", Var(to), Number(num)) => Number(num)
        case BinOp("*", left, right) => BinOp("+", BinOp("*", derive(left, to), right)
          , BinOp("*", left, derive(right, to)))
        case (BinOp("/", Var(to), Number(num))) => Number(1/num)
        case (BinOp("/", Var(to), num)) => BinOp("/", Number(1), derive(num, to))
        case (BinOp("/", num, den)) => BinOp("/", BinOp("-", BinOp("*", derive(num, to), den)
          , BinOp("*", num, derive(num, to))), BinOp("*", den, den))
        case (BinOp("+", left, right)) => BinOp("+", derive(left, to), derive(right, to))
        case (BinOp("-", left, right)) => BinOp("-", derive(left, to), derive(right, to))
      }
    }
  }


