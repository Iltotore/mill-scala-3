package mill.define

import scala.quoted.*

def taskImpl[T](expr: Expr[TaskContext ?=> T])(using Quotes): Expr[Task[T]] =

  import quotes.reflect.*

  //TODO task name
//  report.error(expr.asTerm.show(using Printer.TreeStructure))

  val taskApply = TypeRepr.of[Task].appliedTo(Nil)



  '{???}