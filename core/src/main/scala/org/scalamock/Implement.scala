package org.scalamock

import scala.language.reflectiveCalls

object Implement {
  import language.experimental.macros
  
  def implement[T]: Any = macro ImplementImpl.implement[T]
}

object ImplementImpl {
  import reflect.macros.whitebox.Context
  
  def implement[T: c.WeakTypeTag](c: Context): c.Expr[Any] = {
    import c.universe._
    
    def isMemberOfObject(m: Symbol) = TypeTag.Object.tpe.member(m.name) != NoSymbol
    
    val typeToImplement = weakTypeOf[T]
    val methodsToImplement = typeToImplement.members filter { m => 
        m.isMethod && !isMemberOfObject(m)
      }
    
    val methods = methodsToImplement map { m =>
        val info = m.infoIn(typeToImplement)
        val name = m.name.toTermName
        val resTpe = info.finalResultType
        val tparams = info.typeParams.map(_.name)

        val paramss = info.paramLists.map { ps =>
            ps.map(p => q"${p.name.toTermName}: ${p.infoIn(typeToImplement)}")
          }
        q"def $name[..$tparams](...$paramss): $resTpe = null.asInstanceOf[$resTpe]"
      }
    
    c.Expr[Any](q"""
        new ${typeToImplement} {
          ..$methods
        }
      """)
  }
}
