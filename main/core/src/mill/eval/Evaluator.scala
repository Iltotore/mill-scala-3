package mill.eval

import mill.api.Result
import os.Path

import scala.collection.mutable
import mill.define.{Graph, Task, TaskContext}
import mill.api.Strict.Agg
import mill.api.Result
import mill.util.MultiBiMap

import java.util.logging.Logger

class Evaluator(homePath: Path, outPath: Path, logger: Logger):

  def sequentialEvaluate(goals: Agg[Task[?]]): collection.Map[Task[?], Result[?]] =
    val transitive = Graph.transitiveTargets(goals)
    val topoSorted = Graph.topoSorted(transitive)
    val loggedCount = topoSorted.values.iterator.toSet.count(_.isInstanceOf[Task.Named[?]])

    val (results, evaluated) = evaluateGroup(topoSorted.values, Map.empty, loggedCount, 0)

    results

  def evaluateGroup(
      group: Agg[Task[?]],
      results: collection.Map[Task[?], Result[?]],
      count: Int,
      current: Int
  ): (mutable.LinkedHashMap[Task[?], Result[?]], mutable.Buffer[Task[?]]) =
    val newEvaluated = mutable.Buffer.empty[Task[?]]
    val nonEvaluatedTargets = group.indexed.filterNot(results.contains)
    val newResults = mutable.LinkedHashMap.empty[Task[?], Result[?]]
    var logged = current

    for task <- nonEvaluatedTargets do
      if !newEvaluated.contains(task) then

        val inputValues =
          task
            .inputs
            .map { x => newResults.getOrElse(x, results(x)) }
            .collect { case Result.Success(value) => value }

        val result =
          if inputValues.size == task.inputs.size then
            task match
              case Task.Named(name, _) =>
                logged += 1
                logger.info(s"\u001b[36m[$logged/$count] $name\u001b[0m")
              case _ =>

            val ctx = TaskContext(outPath / task.id, logger, results.toMap ++ newResults, inputValues)
            task.evaluate(ctx)
          else
            Result.Skipped

        newEvaluated.append(task)
        newResults.put(task, result)

    (newResults, newEvaluated)
