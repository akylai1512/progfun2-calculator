package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map{
      case(name, s)=>name ->Signal{
        eval(s(), namedExpressions)
      }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
    {
      def evalRefWithPath(expr: Expr, path:List[Expr]):Double =expr match{
        case Ref(name) if path.contains(expr) =>Double.NaN
        case Ref(name) =>evalRefWithPath(getReferenceExpr(name, references),expr::path )
        case Literal(v) =>v
        case Plus(a,b) =>evalRefWithPath(a,path)+evalRefWithPath(b, path)
        case Minus(a,b) =>evalRefWithPath(a,path)- evalRefWithPath(b, path)
        case Times(a,b) =>evalRefWithPath(a,path)* evalRefWithPath(b, path)
        case Divide(a,b) =>evalRefWithPath(a,path)/evalRefWithPath(b, path)
      }
      evalRefWithPath(expr, Nil)
    }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
