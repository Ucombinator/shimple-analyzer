package org.ucombinator.jaam.tools

import java.io.{File, FileInputStream}
import java.lang.reflect.{Constructor, Method, Modifier}
import java.net.URLClassLoader
import java.util.jar.JarFile

import org.ucombinator.jaam.serializer._

import scala.collection.JavaConversions._
import scala.collection.mutable

sealed trait OriginalRepresentation
case class SootMethod(method: soot.SootMethod)              extends OriginalRepresentation
case class ReflectMethod(method: Method)                    extends OriginalRepresentation
case class ReflectConstructor(constructor: Constructor[_])  extends OriginalRepresentation
case class ClassStaticInitializer(callingClass: Class[_])   extends OriginalRepresentation

class ComparableMethod(val originalMethod: OriginalRepresentation) {
  def this(reflectMethod: Method)               = this(ReflectMethod(reflectMethod))
  def this(reflectConstructor: Constructor[_])  = this(ReflectConstructor(reflectConstructor))
  def this(sootStmt: Stmt)                      = this(SootMethod(sootStmt.method))
  def this(loadedClass: Class[_])               = this(ClassStaticInitializer(loadedClass))

  override def equals(other: Any): Boolean = {
    other match {
      case other: ComparableMethod =>
        this.toString == other.toString
      case _ => false
    }
  }

  override def hashCode: Int = {
    this.toString.hashCode
  }

  override def toString: String = {
    def className(c: Class[_]): String = {
      if (c.isArray) {
        className(c.getComponentType) + "[]"
      } else {
        val canonicalName = c.getCanonicalName
        if (canonicalName == null) {
//          println("className: No canonical name for: " + c.getName)
          c.getName.replace("$", ".")
        } else {
          c.getCanonicalName.replace("$", ".")
        }
      }
    }

    val result = originalMethod match {
      case SootMethod(s) =>
        s.getDeclaringClass.toString.replace("$", ".") +
          " " + s.getReturnType.toString.replace("$", ".") +
          " " + s.getName +
          "(" + s.getParameterTypes.map(t => t.toString.replace("$", ".")).mkString(", ") + ")"
      case ReflectMethod(r) =>
        className(r.getDeclaringClass) +
          " " + className(r.getReturnType) +
          " " + r.getName +
          "(" + r.getParameterTypes.map(className).mkString(", ") + ")"
      case ReflectConstructor(c) =>
        className(c.getDeclaringClass) +
          " " + "void <init>" +
          "(" + c.getParameterTypes.map(className).mkString(", ") + ")"
      case ClassStaticInitializer(i) =>
        className(i) +
          " " + "void <clinit>()"
    }
//    println("toString " + originalMethod + ":" + result)
    result
  }
}

object Coverage {
  // To avoid retyping the Set[Stmt] type all over, here's a type alias.
  // But these need a definition for when `apply` is called, so it's here too.
  type StmtSet = mutable.Set[Stmt]
  def StmtSet(stmts: Stmt*) = mutable.Set[Stmt](stmts: _*)

  private val methodMap = mutable.Map[ComparableMethod, StmtSet]()
  private val jarOnlyMethods  = mutable.Set[ComparableMethod]()
  private val jaamOnlyMethods = mutable.Set[ComparableMethod]()
  private val classEnding = ".class"

  def findCoverage(jaamFile: String, jarFileNames: Seq[String], additionalJarFileNames: Seq[String]) = {
    initializeHashFromJarFiles(jarFileNames, additionalJarFileNames)
    loadStmtsFromJaamFile(jaamFile)
    compareMethods()
  }

  def initializeHashFromJarFiles(jarFileNames: Seq[String], additionalJarFileNames: Seq[String]) = {
    // Iterate over the jarfile names.
    val jarFileURLs = jarFileNames.map(name => new File(name).toURI.toURL).toArray
    val additionalJarFileURLs = additionalJarFileNames.map(name => new File(name).toURI.toURL).toArray
    val classLoader = new URLClassLoader(additionalJarFileURLs ++ jarFileURLs)
    for (jarFileName <- jarFileNames) {
      // Initialize class loader.  Unfortunately, this only works if you know
      // the names of the classes to load... which is unhelpful to us.
      val jarFile = new JarFile(jarFileName, false)
      for (jarEntry <- jarFile.entries().filter(_.getName.endsWith(classEnding))) {
        // The class loader requires the names to be in "binary name" format,
        // e.g. "com.example.MyGreatClass".
        val fileName = jarEntry.getName
        val binaryName = fileName.dropRight(classEnding.length).replaceAll(File.separator, ".")
        try {
          val loadedClass = classLoader.loadClass(binaryName)
          methodMap(new ComparableMethod(loadedClass)) = StmtSet()
          // Exclude abstract classes, since interpretation cannot possibly
          // visit an abstract class.
          loadedClass.getDeclaredMethods.filter(t => (t.getModifiers & Modifier.ABSTRACT) == 0).foreach(m => methodMap(new ComparableMethod(m)) = StmtSet())
          loadedClass.getDeclaredConstructors.foreach(m => methodMap(new ComparableMethod(m)) = StmtSet())
        } catch {
          //TODO: Decide whether to catch these exceptions explicitly.
          case e: Throwable => e.printStackTrace()
        }
      }
    }
  }

  def loadStmtsFromJaamFile(jaamFile: String) = {
    val stream = new FileInputStream(jaamFile)
    val pi = new PacketInput(stream)
    var packet: Packet = null
    while ({packet = pi.read(); !packet.isInstanceOf[EOF]}) {
      packet match {
        case s: State =>
          val method = new ComparableMethod(s.stmt)
          methodMap get method match {
            case Some(_) => methodMap(method) += s.stmt
            case None => () // No corresponding value in map from JAR reading.
              //TODO: Do something with this?
//              println("only in jaam: " + method)
              jaamOnlyMethods += method
          }
        case _ => () // Ignore anything else.
      }
    }
    pi.close()
  }

  def compareMethods() = {
//    var count = 0
    for ((method, stmtSet) <- methodMap) {
      if (stmtSet.isEmpty) {
        jarOnlyMethods += method
//        println("only in jar:  " + method)
//        count += 1
      }
    }
    jaamOnlyMethods.toList.sortBy(_.toString).foreach(m => println("only in jaam: " + m))
    jarOnlyMethods.toList.sortBy(_.toString).foreach( m => println("only in jar:  " + m))
    println("Missing in JAR: " + jaamOnlyMethods.size + " of " + (methodMap.size - jarOnlyMethods.size + jaamOnlyMethods.size))
    println("Missing in JAAM: " + jarOnlyMethods.size + " of " + methodMap.size)
//    println("Missing " + count + " of " + methodMap.size)
  }
}
