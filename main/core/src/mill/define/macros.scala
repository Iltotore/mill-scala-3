package mill.define

import mill.api.Result

import scala.quoted.*

inline def applicative[T](inline f: TaskContext ?=> T): Task[T] =
  ${ applicativeImpl[T]('f) }

def applicativeImpl[T](expr: Expr[TaskContext ?=> T])(using Quotes, Type[T]): Expr[Task[T]] =

  import quotes.reflect.*

  def recTree(tree: Tree): Expr[List[Task[?]]] = tree match
    case statement: Statement => recStatement(statement)
    case caseDef: CaseDef     => recCase(caseDef)
    case _                    => '{ Nil }

  def recCase(caseDef: CaseDef): Expr[List[Task[?]]] = caseDef match
    case CaseDef(_, Some(guard), thenp) =>
      '{ ${ recTerm(guard) } ++ ${ recTerm(thenp) } }
    case CaseDef(_, None, thenp) => recTerm(thenp)

  def recStatement(statement: Statement): Expr[List[Task[?]]] = statement match
    case term: Term             => recTerm(term)
    case definition: Definition => recDef(definition)
    case _                      => '{ Nil }

  def recDef(definition: Definition): Expr[List[Task[?]]] = definition match
    case ValDef(_, _, Some(term))    => recTerm(term)
    case DefDef(_, _, _, Some(term)) => recTerm(term)
    case _                           => '{ Nil }

  def flatMapToExpr[A](list: List[A], f: A => Expr[List[Task[?]]]): Expr[List[Task[?]]] = list match
    case head :: tail => '{ ${ f(head) } ++ ${ flatMapToExpr(tail, f) } }
    case Nil          => '{ Nil }

  def mapOptionToExpr[A](option: Option[A], f: A => Expr[List[Task[?]]]): Expr[List[Task[?]]] = option match
    case Some(value) => f(value)
    case None        => '{ Nil }

  def recTerm(term: Term): Expr[List[Task[?]]] = term match
    case Select(selected, _) => recTerm(selected)
    case NamedArg(_, arg)    => recTerm(arg)
    case Apply(Apply(TypeApply(Select(Ident("Task"), "apply"), _), List(task)), _) =>
      '{ List(${ task.asExprOf[Task[?]] }) }
    case Apply(fun, args) =>
      '{ ${ recTerm(fun) } ++ ${ flatMapToExpr(args, recTerm) } }
    case TypeApply(fun, _) => recTerm(fun)
    case Super(qual, _)    => recTerm(qual)
    case Assign(lhs, rhs)  => '{ ${ recTerm(lhs) } ++ ${ recTerm(rhs) } }
    case Block(statements, expr) =>
      '{ ${ flatMapToExpr(statements, recStatement) } ++ ${ recTerm(expr) } }
    case Closure(meth, _) => recTerm(meth)
    case If(cond, thenp, elsep) =>
      '{ ${ recTerm(cond) } ++ ${ recTerm(thenp) } ++ ${ recTerm(elsep) } }
    case Match(selector, cases) =>
      '{ ${ flatMapToExpr(cases, recCase) } ++ ${ recTerm(selector) } }
    case SummonFrom(cases) => flatMapToExpr(cases, recCase)
    case Try(expr, cases, finalizer) => '{
        ${ recTerm(expr) } ++ ${ flatMapToExpr(cases, recCase) } ++ ${ mapOptionToExpr(finalizer, recTerm) }
      }
    case Return(expr, _)    => recTerm(expr)
    case Repeated(terms, _) => flatMapToExpr(terms, recTerm)
    case Inlined(tree, defs, expr) =>
      '{
        ${ mapOptionToExpr(tree, recTree) } ++ ${ flatMapToExpr(defs, recDef) } ++ ${ recTerm(expr) }
      }
    case SelectOuter(expr, _, _) => recTerm(expr)
    case While(cond, expr)       => '{ ${ recTerm(cond) } ++ ${ recTerm(expr) } }
    case Typed(expr, _)          => recTerm(expr)
    case _                       => '{ Nil }

  '{ Task.Literal(${ recTerm(expr.asTerm) }, ctx => Result.Success(${ expr }(using ctx))) }
