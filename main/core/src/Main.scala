import mill.define.*
import mill.eval.*

import java.util.logging.Logger

object Main:

  given DefineContext = DefineContext(os.pwd, Segment.Label("root"), Segments(), Seq.empty)

  object foo extends Cross[FooModule]("Alice", "Bob", "Ludmila")
  class FooModule(v: String) extends Module:

    def hello = T { println(s"My name is $v") }

  @main def testMain =

    System.setProperty("java.util.logging.SimpleFormatter.format", "%5$s %n");
    val logger = Logger.getLogger("debug")

    val evaluator = Evaluator(os.pwd, os.pwd / "out", logger)

    given ApplyHandler[Task] = Task.evalHandler(evaluator)

    println(foo.name)
    