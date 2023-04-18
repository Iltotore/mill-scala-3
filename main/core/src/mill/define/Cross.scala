package mill.define

import scala.compiletime.{constValue, summonInline}
import scala.quoted.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

/**
 * Models "cross-builds": sets of duplicate builds which differ only in the
 * value of one or more "case" variables whose values are determined at runtime.
 * Used via:
 *
 * object foo extends Cross[FooModule]("bar", "baz", "qux")
 * class FooModule(v: String) extends Module{
 *   ...
 * }
 */
class Cross[T <: Module: ClassTag](cases: Any*)(using ci: Cross.Factory[T], ctx: DefineContext, moduleName: sourcecode.Name)
    extends Module(using ctx, moduleName):

  override lazy val millModuleDirectChildren: Seq[Module] =
    super.millModuleDirectChildren ++
      items.collect { case (_, v: mill.define.Module) => v }

  private val products: List[Product] = cases.toList.map {
    case p: Product => p
    case v          => Tuple1(v)
  }

  val items: List[(List[Any], T)] =
    for (c <- products) yield
      val crossValues = c.productIterator.toList
      val relPath = ctx.segment.pathSegments
      val sub = ci.make(
        c,
        ctx
          .withSegments(ctx.segments ++ Seq(ctx.segment))
          .withMillSourcePath(ctx.millSourcePath / relPath)
          .withSegment(Segment.Cross(crossValues)),
        products
      )
      (crossValues, sub)

  val itemMap: Map[List[Any], T] = items.toMap

  /**
   * Fetch the cross module corresponding to the given cross values
   */
  def get(args: Seq[Any]): T = itemMap(args.toList)

  /**
   * Fetch the cross module corresponding to the given cross values
   */
  def apply(arg0: Any, args: Any*): T = itemMap(arg0 :: args.toList)

  /**
   * Fetch the relevant cross module given the implicit resolver you have in
   * scope. This is often the first cross module whose cross-version is
   * compatible with the current module.
   */
  def apply[V >: T <: Module]()(implicit resolver: Cross.Resolver[V]): T =
    resolver.resolve(this.asInstanceOf[Cross[V]]).asInstanceOf[T]

object Cross:
  case class Factory[T](make: (Product, DefineContext, Seq[Product]) => T):
    private def copy[U](make: (Product, DefineContext, Seq[Product]) => U = make): Factory[U] =
      new Factory[U](make)

  object Factory:

    //TODO Impossible without macro because new T with overriding
    //https://github.com/com-lihaoyi/mill/blob/785e8783bd67cfaa48c2feb0774cac206e16722f/main/core/src/mill/define/Cross.scala#L27

    inline given make[T <: AnyRef]: Factory[T] = ${makeImpl[T]}

    def makeImpl[T <: AnyRef : Type](using Quotes): Expr[Factory[T]] =
      import quotes.reflect.*

      val repr = TypeRepr.of[T]

      repr.dealias.classSymbol match
        case Some(typeSymbol) =>
          val fields =
            typeSymbol
              .primaryConstructor
              .paramSymss
              .headOption
              .getOrElse(Nil)

          def termInstanceOf(term: Term, to: TypeTree): Term =
            TypeApply(Select.unique(term, "asInstanceOf"), List(to))

          def typeOfField(symbol: Symbol): TypeTree =
            symbol.tree match
              case valDef: ValDef => valDef.tpt
              case other => report.errorAndAbort(s"Not a ValDef: ${symbol.tree}")

          def argN(product: Term, tpe: TypeTree, index: Int): Term =
            termInstanceOf(Apply(Select.unique(product, "productElement"), List(Literal(IntConstant(index)))), tpe)

          def args(productTerm: Term): List[Term] = fields.zipWithIndex.map((field, i) => argN(productTerm, typeOfField(field), i))

          val overridingSymbol = Symbol.newClass(Symbol.spliceOwner, Symbol.freshName(typeSymbol.name), List(repr), _ => Nil, None)  

          def newInstance(clazz: Symbol, args: List[Term]): Term = Apply(Select(New(TypeTree.ref(clazz)), clazz.primaryConstructor), args)
          def newParent(args: List[Term]) = newInstance(typeSymbol, args)
          def newOverriding(args: List[Term]) = newInstance(overridingSymbol, args)

          def overridingConstructorDef(args: List[Term]) = DefDef(overridingSymbol.primaryConstructor, _ => Some(newParent(args)))

          val millOuterCtxSym = overridingSymbol.memberMethod("millOuterCtx").head

          /*${contextTerm.asExprOf[DefineContext]}.withCrossInstances( //Error here missing proxy
            ${allProductsTerm.asExprOf[Seq[Product]]}.map(product => ${newParent(args).asExprOf[T]})
          )*/

          def millOuterCtxDef(contextTerm: Term, allProductsTerm: Term, args: List[Term]) = DefDef(millOuterCtxSym, argss => Some(contextTerm))

          def overridingClassDef(contextTerm: Term, allProductsTerm: Term, args: List[Term]) =
           ClassDef(overridingSymbol, List(newParent(args)), List(millOuterCtxDef(contextTerm, allProductsTerm, args)))

          def instance(productTerm: Term, contextTerm: Term, allProductsTerm: Term) =
            val argsTerm = args(productTerm)
            Block(List(overridingClassDef(contextTerm, allProductsTerm, argsTerm)), newOverriding(Nil))

          val lambda = '{(product: Product, context: DefineContext, allProducts: Seq[Product]) => ${instance('product.asTerm, 'context.asTerm, 'allProducts.asTerm).asExprOf[T]}}

          println(lambda.asTerm.show)

          '{Factory(${lambda.asExprOf[(Product, DefineContext, Seq[Product]) => T]})}

        case None => report.errorAndAbort("This type has no class")

    private def unapply[T](factory: Factory[T]): Option[(Product, DefineContext, Seq[Product]) => T] =
      Some(factory.make)

  trait Resolver[-T <: Module]:
    def resolve[V <: T](c: Cross[V]): V
