import mill._, scalalib._, scalafmt._

trait MillModule extends ScalaModule with ScalafmtModule {
  def scalaVersion = "3.3.1-RC1-bin-20230417-6980b2e-NIGHTLY" //Symbol#newClass is experimental before 3.3.1

  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.9.0",
    ivy"com.lihaoyi::sourcecode:0.3.0"
  )
}

object main extends MillModule {

  object api extends MillModule

  object core extends MillModule {

    def moduleDeps = Seq(api, util)
  }

  object util extends MillModule {

    def moduleDeps = Seq(api)
  }
}