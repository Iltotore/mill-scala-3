package mill.define

import os.Path

import java.lang.reflect.Modifier
import scala.reflect.{ClassTag, classTag}

class Module(using outerCtx: DefineContext, moduleName: sourcecode.Name):

  def name: String = moduleName.value

  def millOuterCtx: DefineContext = outerCtx

  def millSourcePath: Path = outerCtx.millSourcePath / name

  lazy val millModuleDirectTasks: Seq[Task[?]] = reflectAll[Task[?]]

  def millModuleDirectChildren: Seq[Module] = millModuleDirectChildrenImpl

  private lazy val millModuleDirectChildrenImpl: Seq[Module] = reflectNestedObjects[Module]

  // Picked from lihaoyi/mill. This part should be exactly the same since it uses Java reflection
  private def reflect[T: ClassTag](filter: (String) => Boolean): Array[T] =
    val runtimeCls = classTag[T].runtimeClass
    for
      m <- this.getClass.getMethods.sortBy(_.getName)
      n = m.getName
      if filter(n) &&
        m.getParameterCount == 0 &&
        (m.getModifiers & Modifier.STATIC) == 0 &&
        (m.getModifiers & Modifier.ABSTRACT) == 0 &&
        runtimeCls.isAssignableFrom(m.getReturnType)
    yield m.invoke(this).asInstanceOf[T]

  private def reflectAll[T: ClassTag]: Array[T] = reflect(Function.const(true))

  private def reflectSingle[T: ClassTag](label: String): Option[T] = reflect(_ == label).headOption

  // For some reason, this fails to pick up concrete `object`s nested directly within
  // another top-level concrete `object`. This is fine for now, since Mill's Ammonite
  // script/REPL runner always wraps user code in a wrapper object/trait
  private def reflectNestedObjects[T: ClassTag]: Array[T] =
    (reflectAll[T] ++
      this
        .getClass
        .getClasses
        .filter(classTag[T].runtimeClass.isAssignableFrom(_))
        .flatMap(c =>
          c.getFields.find(_.getName == "MODULE$").map(_.get(c).asInstanceOf[T])
        )).distinct
