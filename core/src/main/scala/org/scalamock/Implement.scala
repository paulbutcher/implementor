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
      
    def paramType(t: Type) = TypeTree(t)

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
      
    def methodImpl(m: MethodSymbol): DefDef = {
      val mt = m.typeSignatureIn(typeToImplement)
      val params = buildParams(mt)
      val rt = finalResultType(mt)
      DefDef(
        Modifiers(OVERRIDE),
        m.name, 
        m.typeParams map {t => TypeDef(t)},
        params,
        paramType(rt),
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
      
    def castTo(expr: Tree, t: Type) =
      TypeApply(
        Select(expr, newTermName("asInstanceOf")),
        List(TypeTree(t)))
  
    val methodsToImplement = methodsNotInObject.toList
    val members = methodsToImplement map { m => methodImpl(m) }
      
    val result = castTo(anonClass(members), typeToImplement)

    println("------------")
    println(showRaw(result))
    println("------------")
    println(show(result))
    println("------------")
   
    c.Expr(result)
  }
}