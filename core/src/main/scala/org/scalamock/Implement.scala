package org.scalamock

object Implement {
  import language.experimental.macros
  
  def implement[T] = macro ImplementImpl.implement[T]
}

object ImplementImpl {
  import reflect.macros.Context
  
  def implement[T: c.WeakTypeTag](c: Context): c.Expr[T] = {
    import c.universe._
    import Flag._

    val typeToImplement = weakTypeOf[T]

    def finalResultType(methodType: Type): Type = methodType match {
      case NullaryMethodType(result) => result 
      case MethodType(_, result) => finalResultType(result)
      case PolyType(_, result) => finalResultType(result)
      case _ => methodType
    }
    
    def paramss(methodType: Type): List[List[Symbol]] = methodType match {
      case MethodType(params, result) => params :: paramss(result)
      case PolyType(_, result) => paramss(result)
      case _ => Nil
    }

    def getPackage(sym: Symbol): RefTree = {
      val name = newTermName(sym.name.toString)
      if (sym.owner == c.mirror.RootClass)
        Ident(name)
      else
        Select(getPackage(sym.owner), name)
    }

    def fullTypeName(sym: Symbol) = Select(getPackage(sym.owner), newTypeName(sym.name.toString))

    def paramType(t: Type): Tree = {
      val TypeRef(_, sym, params) = t
      if (sym.isParameter)
        Ident(newTypeName(sym.name.toString))
      else if (params.isEmpty)
        fullTypeName(sym)
      else
        AppliedTypeTree(fullTypeName(sym), params map { p => paramType(p) })
    }

    def methodsNotInObject =
      typeToImplement.members filter (m => m.isMethod && !isMemberOfObject(m)) map (_.asMethod)
      
    def buildParams(methodType: Type) =
      paramss(methodType) map { params =>
        params map { p =>
          ValDef(
            Modifiers(PARAM | (if (p.isImplicit) IMPLICIT else NoFlags)),
            newTermName(p.name.toString),
            paramType(p.typeSignature),
            EmptyTree)
        }
      }

    def buildTypeParams(m: MethodSymbol) = {
      m.typeParams map { t =>
        val TypeBounds(lo, hi) = t.typeSignature.asSeenFrom(typeToImplement, typeToImplement.typeSymbol)
        TypeDef(Modifiers(PARAM), 
          newTypeName(t.name.toString),
          List(), 
          TypeBoundsTree(TypeTree(lo), TypeTree(hi)))
      }
    }
      
    def methodImpl(m: MethodSymbol): DefDef = {
      val mt = m.typeSignatureIn(typeToImplement)
      val tparams = buildTypeParams(m)
      val params = buildParams(mt)
      val rt = paramType(finalResultType(mt))
      DefDef(
        Modifiers(OVERRIDE),
        m.name, 
        tparams,
        params,
        rt,
        castTo(Literal(Constant(null)), rt))
    }
      
    def initDef = 
      DefDef(
        Modifiers(), 
        newTermName("<init>"), 
        List(), 
        List(List()), 
        TypeTree(),
        Block(
          Apply(
            Select(Super(This(newTypeName("")), newTypeName("")), newTermName("<init>")), 
            List())))
        
    def isMemberOfObject(m: Symbol) = TypeTag.Object.tpe.member(m.name) != NoSymbol
  
    def anonClass(members: List[Tree]) =
      Block(
        List(
          ClassDef(
            Modifiers(FINAL), 
            newTypeName("$anon"),
            List(),
            Template(
              List(TypeTree(typeToImplement)), 
              emptyValDef,
              initDef +: members))),
        Apply(
          Select(
            New(Ident(newTypeName("$anon"))), 
            newTermName("<init>")), 
          List()))
      
    def castTo(expr: Tree, t: Tree) =
      TypeApply(
        Select(expr, newTermName("asInstanceOf")),
        List(t))
  
    val methodsToImplement = methodsNotInObject.toList
    val members = methodsToImplement map { m => methodImpl(m) }
      
    val result = castTo(anonClass(members), TypeTree(typeToImplement))

    println("------------")
    println(showRaw(result))
    println("------------")
    println(show(result))
    println("------------")
   
    c.Expr(result)
  }
}