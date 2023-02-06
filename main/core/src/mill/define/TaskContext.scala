package mill.define

import scala.language.implicitConversions
import os.Path

import java.util.logging.{Level, Logger}
import scala.annotation.implicitNotFound

@implicitNotFound("This method is only usable in a task or given a `TaskContext`")
case class TaskContext(wd: Path, logger: Logger)

object TaskContext:

  enum TaskControl(message: String) extends Exception(message: String):
    case Failure(message: String) extends TaskControl(message)
    case Skipped extends TaskControl("skipped")
    case Aborted extends TaskControl("aborted")
