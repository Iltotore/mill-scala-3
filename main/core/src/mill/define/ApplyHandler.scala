package mill.define

trait ApplyHandler[M[+_]]:

  def apply[T](t: M[T]): T
