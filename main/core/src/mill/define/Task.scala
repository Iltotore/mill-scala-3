package mill.define

import mill.api.Result
import mill.api.Strict.Agg
import mill.eval.Evaluator

import java.util.logging.Level
import scala.annotation.targetName
import T.unboxResult

enum Task[+T]:
  case Literal(inputs: Seq[Task[?]], body: TaskContext => Result[T])
  case Mapped[A, +B](source: Task[A], f: TaskContext => A => Result[B]) extends Task[B]
  case Sequence[+A](tasks: Seq[Task[A]]) extends Task[Seq[A]]
  case Named(name: String, underlying: Task[T])

  override def hashCode(): Int = this match
    case Named(name, _) => name.hashCode()
    case task           => super.hashCode()

  override def equals(x: Any): Boolean = (this, x) match
    case (Named(thisName, _), Named(xName, _)) => thisName == xName
    case task                                  => super.equals(x)

object Task:

  import Task.*
  import TaskContext.TaskControl

  def evalHandler(evaluator: Evaluator): ApplyHandler[Task] = new ApplyHandler[Task]:

    override def apply[T](task: Task[T]): T =
      evaluator
        .sequentialEvaluate(Agg(task))(task)
        .get
        .asInstanceOf[T]

  given (using ctx: TaskContext): ApplyHandler[Task] with

    override def apply[T](task: Task[T]): T =
      ctx.results.get(task) match
        case Some(Result.Success(value)) => value.asInstanceOf[T]
        case Some(_)                     => throw TaskControl.Skipped
        case _ =>
          println(ctx.results)
          throw TaskControl.Failure(s"No result found for task $task")

  inline def create[T](inline body: TaskContext ?=> T)(using name: sourcecode.Name): Task[T] =
    Named(name.value, applicative(body))

  extension [T](task: Task[T])

    def apply()(using handler: ApplyHandler[Task]): T = handler(task)

    def evaluate(ctx: TaskContext): Result[T] = task match
      case Literal(_, body) =>
        try
          body(ctx)
        catch
          case TaskControl.Failure(msg) => Result.Failure(msg)
          case TaskControl.Skipped      => Result.Skipped
          case TaskControl.Aborted      => Result.Aborted
      case Mapped(_, f)   => Result.Success(ctx(0))
      case Sequence(_)    => Result.Success(ctx.values.asInstanceOf[T])
      case Named(name, _) => Result.Success(ctx(0))

    def flatMap[U](f: T => Result[U]): Task[U] = Task.Mapped(task, _ => f)

    def map[U](f: T => U): Task[U] = task.flatMap(x => Result.Success(f(x)))

    def inputs: Seq[Task[?]] = task match
      case Literal(inputs, _)   => inputs
      case Mapped(source, _)    => List(source)
      case Sequence(tasks)      => tasks
      case Named(_, underlying) => List(underlying)

    def id: String = task match
      case Named(name, _) => name
      case task           => task.hashCode().toString

    