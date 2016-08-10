package org.ucombinator.jaam.interpreter

import scala.collection.JavaConversions._

import soot.{Main => SootMain, Unit => SootUnit, Value => SootValue, _}
import soot.jimple.{Stmt => SootStmt, _}

import soot.options.Options
import soot.tagkit.{GenericAttribute, SourceFileTag}

import org.ucombinator.jaam.serializer
import org.ucombinator.jaam.interpreter.Stmt.unitToStmt // Automatically convert soot.Unit to soot.Stmt

// Helpers for working with Soot.
//
// Note that these methods relate to Soot only and do not include any
// classes or logic for the analyzer

object Stmt {
  val indexTag = "org.ucombinator.jaam.Stmt.indexTag"

  import scala.language.implicitConversions
  implicit def unitToStmt(unit : SootUnit) : SootStmt = {
    assert(unit ne null, "unit is null")
    assert(unit.isInstanceOf[SootStmt], "unit not instance of Stmt. Unit is of class: " + unit.getClass)
    unit.asInstanceOf[SootStmt]
  }

  def getIndex(sootStmt : SootStmt, sootMethod : SootMethod) : Int = {
    if (sootStmt.hasTag(Stmt.indexTag)) {
      BigInt(sootStmt.getTag(Stmt.indexTag).getValue).intValue
    } else {
      // label everything in the sootMethod so the amortized work is linear
      for ((u, i) <- Soot.getBody(sootMethod).getUnits().toList.zipWithIndex) {
        u.addTag(new GenericAttribute(Stmt.indexTag, BigInt(i).toByteArray))
      }

      assert(sootStmt.hasTag(Stmt.indexTag), "SootStmt "+sootStmt+" not found in SootMethod " + sootMethod)
      BigInt(sootStmt.getTag(Stmt.indexTag).getValue).intValue
    }
  }

  def methodEntry(sootMethod : SootMethod) = Stmt(Soot.getBody(sootMethod).getUnits.getFirst, sootMethod)
}

case class Stmt(val sootStmt : SootStmt, val sootMethod : SootMethod) {
  val index = Stmt.getIndex(sootStmt, sootMethod)
  val line = sootStmt.getJavaSourceStartLineNumber
  val column = sootStmt.getJavaSourceStartColumnNumber
  val sourceFile = {
    val tag = sootMethod.getDeclaringClass.getTag("SourceFileTag")
    if (tag != null) {
      val sourceTag = tag.asInstanceOf[SourceFileTag]
      if (sourceTag.getAbsolutePath != null) { sourceTag.getAbsolutePath + "/" } else { "" } + sourceTag.getSourceFile
    } else { "<unknown>" }
  }

  def toPacket() : serializer.Stmt = serializer.Stmt(sootMethod, index, sootStmt)
  def nextSyntactic : Stmt = this.copy(sootStmt = Soot.getBody(sootMethod).getUnits().getSuccOf(sootStmt))
  def nextSemantic : List[Stmt] =
    sootStmt match {
      case sootStmt : ReturnStmt => List()
      case sootStmt : ReturnVoidStmt => List ()
      case sootStmt : ThrowStmt => List ()
      case sootStmt : GotoStmt => List(this.copy(sootStmt = sootStmt.getTarget))
      case sootStmt : SwitchStmt => sootStmt.getTargets.toList.map(u => this.copy(sootStmt = u))
      case sootStmt : IfStmt => List(this.nextSyntactic, this.copy(sootStmt = sootStmt.getTarget))
      case sootStmt => List(this.nextSyntactic)
    }

  override def toString : String = sootMethod + ":" + index + ":" + sootStmt
}

object Soot {
  def initialize(config : Config) {
    Options.v().set_verbose(false)
    // Put class bodies in Jimple format
    Options.v().set_output_format(Options.output_format_jimple)
    // Process all packages and do not exclude packages like java.*
    Options.v().set_include_all(true)
    // we need to link instructions to source line for display
    Options.v().set_keep_line_number(true)
    // Called methods without jar files or source are considered phantom
    Options.v().set_allow_phantom_refs(true)
    // Use the class path from the command line
    Options.v().set_soot_classpath(config.rtJar + ":" + config.sootClassPath)
    // Use only the class path from the command line
    Options.v().set_prepend_classpath(false)
    // Take definitions only from class files
    Options.v().set_src_prec(Options.src_prec_only_class)

    // Compute dependent options
    soot.Main.v().autoSetOptions()

    // Run transformations and analyses according to the configured options.
    // Transformation could include jimple, shimple, and CFG generation
    PackManager.v().runPacks()
    Scene.v().loadBasicClasses()
  }

  def getBody(m : SootMethod) = {
    if (m.isNative) { throw new Exception("Unimplemented native method: " + m) }
    if (!m.hasActiveBody()) {
      SootResolver.v().resolveClass(m.getDeclaringClass.getName, SootClass.BODIES)
      m.retrieveActiveBody()
    }
    m.getActiveBody
  }

  def isJavaLibraryClass(sootClass: SootClass) = {
    sootClass.isJavaLibraryClass || sootClass.getName.startsWith("org.w3c")
  }

  def getSootClass(s : String) = Scene.v().loadClass(s, SootClass.SIGNATURES)

  def getSootType(t : String): Type = t match {
    case "int" => soot.IntType.v()
    case "bool" => soot.BooleanType.v()
    case "double" => soot.DoubleType.v()
    case "float" => soot.FloatType.v()
    case "long" => soot.LongType.v()
    case "byte" => soot.ByteType.v()
    case "short" => soot.ShortType.v()
    case "char" => soot.CharType.v()
    case _ => soot.RefType.v(t)
  }

  def isPrimitive(t : Type) : Boolean = !t.isInstanceOf[RefLikeType]

  def isSubclass(sub : SootClass, sup : SootClass) : Boolean = {
    return sub == sup ||
     ((sub.isInterface, sup.isInterface) match {
      case (false, false) => Scene.v().getActiveHierarchy.isClassSubclassOf(sub, sup)
      case (true, true) => Scene.v().getActiveHierarchy.isInterfaceSubinterfaceOf(sub, sup)
      case (false, true) => {
        val h = Scene.v().getActiveHierarchy()
        h.getSuperclassesOfIncluding(sub)
          .exists(_.getInterfaces().exists(h.getSuperinterfacesOfIncluding(_).exists(_ == sup)))
      }
       case (true, false) => false
     })
  }

  object classes {
    lazy val Object = getSootClass("java.lang.Object")
    lazy val Class = getSootClass("java.lang.Class")
    lazy val String = getSootClass("java.lang.String")
    lazy val Clonable = getSootClass("java.lang.Clonable")
    lazy val ClassCastException = getSootClass("java.lang.ClassCastException")
    lazy val ArithmeticException = getSootClass("java.lang.ArithmeticException")
    lazy val Serializable = getSootClass("java.io.Serializable")
  }

  // is a of type b?
  def isSubType(a : Type, b : Type) : Boolean = {
    if (a equals b) {
      return true
    } else {
      if (isPrimitive(a) || isPrimitive(b)) {
        return false
      } else {
        (a, b) match {
          case (at : ArrayType, bt : ArrayType) => {
            (at.numDimensions == bt.numDimensions) &&
              isSubType(at.baseType, bt.baseType)
          }
          case (ot : Type, at : ArrayType) => {
            ot.equals(classes.Object.getType) ||
              ot.equals(classes.Clonable.getType) ||
              ot.equals(classes.Serializable.getType)
          }
          case (at : ArrayType, ot : Type) => {
            Log.warn("Checking if a non-array type "+ot+" is an array")
            false // maybe
          }
          case _ => {
            val lub: Type = a.merge(b, Scene.v)
            val result = (lub != null) && !lub.equals(a)
            return result
          }
        }
      }
    }
  }
}
