package mill.define

object Playground extends Module {

    object core extends Module

    object api extends Module {
        object a
        object b extends Module
        object c extends Module
    }

    def main(args: Array[String]): Unit = {
        val modules = getModules[Playground.type]
        println(modules)
    }

}
