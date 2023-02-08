package mill.define

import java.util.logging.Level
import scala.annotation.targetName

enum Task[+T]:
  case Literal(inputs: Seq[Task[?]], body: TaskContext => Result[T])
  case Mapped[A, +B](source: Task[A], f: TaskContext => A => Result[B]) extends Task[B]
  case Sequence[+A](tasks: Seq[Task[A]]) extends Task[Seq[A]]
  case Named(name: String, underlying: Task[T])

object Task:

  import Task.*
  import TaskContext.TaskControl

  inline def create[T](inline body: TaskContext ?=> T)(using name: sourcecode.Name): Task[T] =
    Named(name.value, applicative(body))

  extension [T](task: Task[T])

    def apply()(using ctx: TaskContext): Result[T] =
      task match
        case Literal(inputs, body) =>
          try
            body(ctx)
          catch
            case TaskControl.Failure(msg) => Result.Failure(msg)
            case TaskControl.Skipped => Result.Skipped
            case TaskControl.Aborted => Result.Aborted

        case Mapped(source, f) =>
          val srcResult = source()

          try
            srcResult.flatMap(f(ctx))
          catch
            case TaskControl.Failure(msg) => Result.Failure(msg)
            case TaskControl.Skipped => Result.Skipped
            case TaskControl.Aborted => Result.Aborted

        case Sequence(tasks) =>
          val buffer = scala.collection.mutable.ListBuffer.empty[T]
          ???

        case Named(name, underlying) =>
          T.log(s"Executing $name...")
          underlying()

    def flatMap[U](f: T => Result[U]): Task[U] = Task.Mapped(task, _ => f)

    def map[U](f: T => U): Task[U] = task.flatMap(x => Result.Success(f(x)))

    def inputs: Seq[Task[?]] = task match
      case Literal(inputs, _) => inputs
      case Mapped(source, _) => source.inputs
      case Sequence(tasks) => tasks.flatMap(_.inputs)
      case Named(_, underlying) => underlying.inputs
    

