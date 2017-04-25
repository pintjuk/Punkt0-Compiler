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
    def superType():Type = this;
    def lub(other: Type):Type = TUntyped;

  }
 
  trait lubed {
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

    override def superType():Type = {
      classSymbol.parent match{
        case None =>  anyRef
        case Some(sym) => sym.getType match {
          case t:TClass => t
          case t        => throw new IllegalStateException("WTF! type of class symbol must be a class, how is this even possible?");
        }
      }
    }

    override def lub(other: Type):Type = {
      if(other==this)
        this
      else if(other == superType)
        other
      else if(other.superType == this)
        this
      else 
        other.superType.lub(this.superType)
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

    override def lub(other: Type):Type={
      other match {
        case oth:TAnyRef => anyRef;
        case oth:TClass  => anyRef;
        case oth         => TUntyped;
      }
    }

    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TClass(new ClassSymbol("AnyRef"))
}
