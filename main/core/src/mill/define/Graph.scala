package mill.define

import mill.eval.Tarjans
import mill.util.MultiBiMap
import mill.api.Strict.Agg

object Graph:

  /**
   * The `values` [[Agg]] is guaranteed to be topological sorted and cycle free.
   * That's why the constructor is package private.
   * @see [[Graph.topoSorted]]
   */
  class TopoSorted private[Graph] (val values: Agg[Task[?]])

  def groupAroundImportantTargets(topoSortedTargets: TopoSorted)(
      important: PartialFunction[
        Task[?],
        Task[?]
      ]
  ): MultiBiMap[Task[?], Task[?]] =

    val output = new MultiBiMap.Mutable[Task[?], Task[?]]()
    for
      (target, t) <- topoSortedTargets.values.flatMap(t =>
        important.lift(t).map((t, _))
      ).iterator
    do

      val transitiveTargets = new Agg.Mutable[Task[?]]
      def rec(t: Task[?]): Unit =
        if transitiveTargets.contains(t) then () // do nothing
        else if important.isDefinedAt(t) && t != target then () // do nothing
        else {
          transitiveTargets.append(t)
          t.inputs.foreach(rec)
        }
      rec(target)
      output.addAll(t, topoSorted(transitiveTargets).values)
    output

  /**
   * Collects all transitive dependencies (targets) of the given targets,
   * including the given targets.
   */
  def transitiveTargets(sourceTargets: Agg[Task[?]]): Agg[Task[?]] =
    transitiveNodes(sourceTargets)

  /**
   * Collects all transitive dependencies (nodes) of the given nodes,
   * including the given nodes.
   */
  def transitiveNodes(sourceNodes: Agg[Task[?]]): Agg[Task[?]] =
    val transitiveNodes = new Agg.Mutable[Task[?]]
    def rec(t: Task[?]): Unit =
      if transitiveNodes.contains(t) then {} // do nothing
      else {
        transitiveNodes.append(t)
        t.inputs.foreach(rec)
      }

    sourceNodes.items.foreach(rec)
    transitiveNodes

  /**
   * Takes the given targets, finds all the targets they transitively depend
   * on, and sort them topologically. Fails if there are dependency cycles
   */
  def topoSorted(transitiveTargets: Agg[Task[?]]): TopoSorted =

    val indexed = transitiveTargets.indexed
    val targetIndices = indexed.zipWithIndex.toMap

    val numberedEdges =
      for (t <- transitiveTargets.items)
        yield t.inputs.collect(targetIndices)

    val sortedClusters = Tarjans(numberedEdges)
    val nonTrivialClusters = sortedClusters.filter(_.length > 1)
    assert(nonTrivialClusters.isEmpty, nonTrivialClusters)
    new TopoSorted(Agg.from(sortedClusters.flatten.map(indexed)))
