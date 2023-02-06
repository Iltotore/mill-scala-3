import mill._, scalalib._

trait MillModule extends ScalaModule {
  def scalaVersion = "3.2.2"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.9.0",
    ivy"com.lihaoyi::sourcecode:0.3.0"
  )
}

object main extends MillModule {

  object core extends MillModule

}