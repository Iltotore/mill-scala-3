package mill.api

import mill.define.TaskContext.TaskControl

enum Result[+T]:
  case Success(value: T)
  case Failure(message: String) extends Result[Nothing]
  case Skipped extends Result[Nothing]
  case Aborted extends Result[Nothing]

import Result.*

object Result:
  extension [T](result: Result[T])

    def map[U](f: T => U): Result[U] = result match
      case Success(value)   => Success(f(value))
      case Failure(message) => Failure(message)
      case Skipped          => Skipped
      case Aborted          => Aborted

    def flatMap[U](f: T => Result[U]): Result[U] = result match
      case Success(value)   => f(value)
      case Failure(message) => Failure(message)
      case Skipped          => Skipped
      case Aborted          => Aborted

    def get: T = result match
      case Success(value)   => value
      case Failure(message) => throw TaskControl.Failure(message)
      case Skipped          => throw TaskControl.Skipped
      case Aborted          => throw TaskControl.Aborted
