package punkt0
package analyzer

import Symbols._

case class primitivLubException(
  message: String = "Cant Lub of a primitive type with any other type then itself is undefined",
  cause: Throwable = None.orNull
  ) extends Exception(message, cause)

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
    def superType():Type = this
    def lub(other: Type):Type

  }
 
  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
    override def lub(other: Type):Type = ???
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
    override def lub(other: Type):Type = ???
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false

    }
    override def lub(other: Type):Type = other match {
      case TInt => TInt
      case _    => throw new primitivLubException
    }
    override def toString = "Int"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    
    override def lub(other: Type):Type = other match {
      case TBoolean => TBoolean
      case _        => throw new primitivLubException
    }
    override def toString = "Boolean"

  }

  case object TString extends Type{
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    
    override def lub(other: Type):Type = other match {
      case TString => TString
      case _      => throw new primitivLubException
    }
    override def toString = "String"

  }

  case object TUnit extends Type{
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ => false
    }
    override def lub(other: Type):Type = other match {
      case TUnit  => TUnit
      case _      => throw new primitivLubException
    }
    override def toString = "Unit"

  }

  case class TClass (classSymbol: ClassSymbol) extends Type{
    def name = classSymbol.name
    def ==(that:Type): Boolean = {
      that match {
        case v:TClass => v.classSymbol.name == name
        case _        => false
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
      if(other.isSubTypeOf(this))
        this
      else if(this.isSubTypeOf(other))
        other
      else 
        other.superType.lub(this.superType)
    }

    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case Types.anyRef => true
      case Types.bottomRef => false
      case otherClass : TClass => if (otherClass.classSymbol.name==classSymbol.name){
                                    true
                                  }
                                  else classSymbol.parent match {
                                    case None    => false
                                    case Some(v) => v.getType.isSubTypeOf(otherClass)
                                  }
      case _ => false
    }
    override def toString = name

  }

  case class TAnyRef(classSymbol: ClassSymbol) extends Type {
    def ==(that:Type): Boolean = {
      that match {
        case _:TAnyRef  => true
        case _          => false
      }
    }

    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case Types.anyRef => true
      case _ => false
    }

    override def lub(other: Type):Type={
      other match {
        case Types.anyRef     => Types.anyRef;
        case Types.bottomRef  => Types.bottomRef;
        case _:TClass       => anyRef;
        case _              => throw new RuntimeException("Cant lub on non reference types");
      }
    }

    override def toString = classSymbol.name
  }
  case class TBottom(classSymbol: ClassSymbol) extends Type {
    def ==(that:Type): Boolean = {
      that match {
        case _:TBottom => true
        case _          => false
      }
    }

    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case Types.bottomRef  => true
      case _:TClass         => true
      case Types.anyRef     => true
      case _                => false
    }

    override def lub(other: Type):Type={
      other match {
        case Types.anyRef    => anyRef;
        case oth:TClass     => oth;
        case Types.bottomRef => bottomRef;
        case _         => throw new RuntimeException("cant lub with non reference types");
      }
    }

    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))
  val bottomRef = TBottom(new ClassSymbol("bottomRef"))
}
