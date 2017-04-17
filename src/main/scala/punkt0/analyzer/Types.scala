package punkt0
package analyzer

import Symbols._

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "Int"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "Boolean"

  }

  case object TString extends Type{
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "String"

  }

  case object TUnit extends Type{
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ => false
    }
    override def toString = "Unit"

  }

  case class TClass (classSymbol: ClassSymbol) extends Type{
    def name = classSymbol.name
    def ==(that:Type): Boolean = {
      that match {
        case v:TClass => v.classSymbol.name == name
        case v        => false
      }
    }

    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case Types.anyRef => true
      case otherClass : TClass => if (otherClass.classSymbol.name==classSymbol.name){
                                    true
                                  }
                                  else classSymbol.parent match {
                                    case None    => false
                                    case Some(v) => v.getType.isSubTypeOf(otherClass)
                                  }
      case _ => false
    }
    override def toString = "String"

  }

  case class TAnyRef(classSymbol: ClassSymbol) extends Type {
    def ==(that:Type): Boolean = {
      that match {
        case v:TAnyRef  => true
        case v          => false
      }
    }

    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case Types.anyRef => true
      case _ => false
    }

    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))
}
