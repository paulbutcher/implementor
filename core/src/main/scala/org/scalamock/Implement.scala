package org.scalamock

import scala.language.reflectiveCalls

object Implement {
  import language.experimental.macros
  
  def implement[T]: Any = macro ImplementImpl.implement[T]
}

object ImplementImpl {
  import reflect.macros.whitebox.Context
  
  def implement[T: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    
    val tpe = weakTypeOf[T]
    val targs = tpe.typeArgs.map(_.typeSymbol)
    val tparms = tpe.typeConstructor.typeSymbol.asType.typeParams

    def objMember(m: Symbol) = typeOf[Object].member(m.name) != NoSymbol

    val methods = tpe.members.collect {
      case m if m.isMethod && !objMember(m) => m.asMethod
    }

    val impls = methods map { m =>
      val name = m.name.toTermName
      val tparams = m.typeParams.map(c.internal.typeDef(_))
      val paramss = m.paramLists.map(_.map(p => q"val ${p.name.toTermName}: ${p.info}"))
      val resTpe = m.returnType

      val defn =
        q"def $name[..$tparams](...$paramss): $resTpe = null.asInstanceOf[$resTpe]"
      c.internal.substituteSymbols(defn, tparms, targs)
    }
    
    val inst = TypeName(c.freshName)

    q"""
      class $inst extends $tpe {
        ..$impls
      }
      new $inst
    """
  }
}
