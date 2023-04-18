package mill.define

import os.Path

case class DefineContext(
  millSourcePath: Path,
  segment: Segment,
  segments: Segments,
  crossInstances: Seq[AnyRef]
):

  def withMillSourcePath(millSourcePath: Path): DefineContext = copy(millSourcePath = millSourcePath)

  def withSegment(segment: Segment): DefineContext = copy(segment = segment)

  def withSegments(segments: Segments): DefineContext = copy(segments = segments)

  def withCrossInstances(crossInstances: Seq[AnyRef]): DefineContext = copy(crossInstances = crossInstances)
