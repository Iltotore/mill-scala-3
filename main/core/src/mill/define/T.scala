package mill.define

import mill.api.Result

import scala.language.implicitConversions
import os.Path

import java.util.logging.Level

object T:

  import TaskContext.TaskControl

  export Task.{create as apply}

  implicit def unboxResult[T](result: Result[T])(using TaskContext): T =
    result match
      case Result.Success(value) => value
      case Result.Failure(msg)   => throw TaskControl.Failure(msg)
      case Result.Skipped        => throw TaskControl.Skipped
      case Result.Aborted        => throw TaskControl.Aborted

  def fail(msg: String)(using TaskContext): Nothing =
    throw TaskControl.Failure(msg)

  def skip(using TaskContext): Nothing = throw TaskControl.Skipped

  def abort(using TaskContext): Nothing = throw TaskControl.Aborted

  def log(msg: String)(using ctx: TaskContext): Unit =
    ctx.logger.log(Level.INFO, msg)

  def dest(using ctx: TaskContext): Path = ctx.dest
