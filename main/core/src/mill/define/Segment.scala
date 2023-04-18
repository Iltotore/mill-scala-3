package mill.define

enum Segment:
  case Label(value: String)
  case Cross(values: Seq[Any])

  def pathSegments: Seq[String] = this match
    case Segment.Label(s)  => Seq(s)
    case Segment.Cross(vs) => vs.map(_.toString)
