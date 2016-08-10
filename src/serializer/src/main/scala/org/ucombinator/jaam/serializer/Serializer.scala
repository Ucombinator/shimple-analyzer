package org.ucombinator.jaam.serializer

/*****************************************
 * This library handles reading and writting ".jaam" files.  For usage see
 * PacketInput and PacketOutput in this package.
 * ****************************************/

import java.lang.Object
import java.io.{InputStream, OutputStream, IOException}
import java.lang.reflect.Type
import java.util.zip.{DeflaterOutputStream, InflaterInputStream}

import scala.collection.JavaConversions._

import soot.SootMethod
import soot.jimple.{Stmt => SootStmt}
import soot.util.Chain
import org.objectweb.asm.tree.{InsnList, AbstractInsnNode}

import com.esotericsoftware.minlog.Log

import com.esotericsoftware.kryo.{Kryo, Registration, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.serializers.FieldSerializer

import org.objenesis.strategy.StdInstantiatorStrategy
import de.javakaffee.kryoserializers.UnmodifiableCollectionsSerializer
import com.twitter.chill.{AllScalaRegistrar, KryoBase, ScalaKryoInstantiator}


////////////////////////////////////////
// 'PacketInput' is used to read a ".jaam" file
//
// Usage of this class:
//   in = new PacketInput(new FileInputStream("<filename>.jaam"))
//   in.read()
class PacketInput(private val input : InputStream) {
  // Reads a 'Packet'
  // TODO: check for exceptions
  def read() : Packet = {
    this.kryo.readClassAndObject(in) match {
      case o : Packet => o
      case o => throw new IOException("Read object is not a Packet: " + o)
    }
  }

  // Closes this 'PacketInput'
  def close() = in.close()

  ////////////////////////////////////////
  // Implementation internals
  ////////////////////////////////////////

  checkHeader("Jaam file-format signature", Signatures.formatSignature)
  checkHeader("Jaam file-format version", Signatures.formatVersion)

  private val in = new Input(new InflaterInputStream(input))
  private val kryo : Kryo = new JaamKryo()

  private def checkHeader(name : String, expected : Array[Byte]) {
    val found = new Array[Byte](expected.length)
    val len = input.read(found, 0, found.length)

    if (len != expected.length) {
      throw new IOException(
        "Reading %s yielded only %d bytes. Expected %d bytes."
          .format(name, len, expected.length))
    }

    if (found.toList != expected.toList) {
      val e = (for (i <- expected) yield { "%x".format(i) }).mkString("")
      val f = (for (i <- found)    yield { "%x".format(i) }).mkString("")
      throw new IOException("Invalid %s\n Expected: 0x%s\n Found:    0x%s"
        .format(name, e, f))
    }
  }
}

////////////////////////////////////////
// 'PacketOutput' is used to write ".jaam" files
//
// Usage of this class:
//   out = new PacketOutput(new FileOutputStream("<filename>.jaam"))
//   out.write(packet)
class PacketOutput(private val output : OutputStream) {
  // Writes a 'Packet'
  def write(m : Packet) : Unit = {
    this.kryo.writeClassAndObject(this.out, m)
  }

  // Flushes output
  def flush() = out.flush()

  // Closes this 'PacketOutput'
  def close() : Unit = {
    this.write(EOF())
    out.close()
  }

  ////////////////////////////////////////
  // Implementation internals
  ////////////////////////////////////////

  output.write(Signatures.formatSignature)
  output.write(Signatures.formatVersion)

  private val out = new Output(new DeflaterOutputStream(output))
  private val kryo : Kryo = new JaamKryo()
}


private[this] object Signatures {
  // File signature using the same style as PNG
  // \x008a = 'J' + 0x40: High bit set so 'file' knows we are binary
  // JAAM: Help humans figure out what format the file is
  // \r\n: Detect bad line conversion
  // \x001a = '^Z': Stops output on DOS
  // \n: Detect bad line conversion
  val formatSignature = "\u008aJAAM\r\n\u001a\n".getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)
  val formatVersion = { val version = 1; java.nio.ByteBuffer.allocate(4).putInt(version).array() }
}


////////////////////////////////////////
// Packet types
////////////////////////////////////////

// The super type of all packets
abstract class Packet {}

// Signals that all packets are done
// TODO: check if "object" is okay here
case class EOF () extends Packet {}

// Declare a transition edge between two 'State' nodes
case class Edge(id : Id[Edge], src : Id[Node], dst : Id[Node]) extends Packet {}

// Declare the Node - the counterpart to the edge
abstract class Node(val id : Id[Node]) extends Packet {}

// Declare 'AbstractState' nodes
abstract class AbstractState(override val id : Id[Node]) extends Node(id) {}
case class ErrorState(override val id : Id[Node]) extends AbstractState(id) {}
case class State(override val id : Id[Node], stmt : Stmt, framePointer : String, kontStack : String) extends AbstractState(id) {}

// Declare 'MissingState' nodes, used by jaam.tools.Validate
case class MissingReferencedState(override val id : Id[Node]) extends Node(id) {}

//case class Group(id : Id[Node], states : java.util.List[Node], labels : String)

//tags for the analyzer
case class NodeTag(id : Id[Tag], node : Id[Node], tag : Tag) extends Packet {}
abstract class Tag {}
case class AllocationTag(val sootType : soot.Type) extends Tag {}


////////////////////////////////////////
// Types inside packets
////////////////////////////////////////

// Identifiers qualified by a namespace
case class Id[Namespace](id : Int) {
  // val namespace = classOf[Namespace]
}

// Type for statements (needed because 'SootStmt' doesn't specify the
// 'SootMethod' that it is in)
case class Stmt(method : SootMethod, index : Int, stmt : SootStmt) {}

/*
Classes that we may eventually need to support in 'Packet':

AbstractState
  ErrorState
  State(Stmt, FramePointer, KontStack)

Stmt(SootStmt, SootMethod)

FramePointer
  InvariantFramePointer
  ZeroCFAFramePointer(SootMethod)
  OneCFAFramePointer(SootMethod, Stmt)
  InitialFramePointer

From
  FromNative
  FromJava

BasePointer
  OneCFABasePointer(Stmt, FramePointer, From)
  InitialBasePointer
  StringBasePointer(String)
  ClassBasePointer(String)

Value
  AtomicValue
    AnyAtomicValue
  ObjectValue(SootClass, BasePointer)
  ArrayValue(SootType, BasePointer)

KontAddr
  OneCFAKontAddr

Addr
  FrameAddr
    LocalFrameAddr(FramePointer, Local)
    ParameterFrameAddr(FramePointer, Int)
    ThisFrameAddr
    CaughtExceptionFrameAddr
  InstanceFieldAddr
  ArrayRefAddr
  ArrayLengthAddr
  StaticFieldAddr

Store
KontStack(Kont)

Kont
  RetKont(Frame, KontAddr)
  HaltKont

Frame(Stmt, FramePointer, Option[Set[Addr]]))
 */

////////////////////////////////////////
// Internal classes
////////////////////////////////////////

// Internal Kryo object that adds extra checking of the types and field
// structures of read and written objects to be sure they match what was
// expected.
class JaamKryo extends KryoBase {
  var seenClasses = Set[Class[_]]()

  // This is copies from Chill
  this.setRegistrationRequired(false)
  this.setInstantiatorStrategy(new StdInstantiatorStrategy)
  // Handle cases where we may have an odd classloader setup like with libjars
  // for hadoop
  val classLoader = Thread.currentThread.getContextClassLoader
  this.setClassLoader(classLoader)
  val reg = new AllScalaRegistrar
  reg(this)
  this.setAutoReset(false)
  this.addDefaultSerializer(classOf[Chain[Object]], classOf[FieldSerializer[java.lang.Object]])
  UnmodifiableCollectionsSerializer.registerSerializers(this)

  // Produces a string that documents the field structure of 'typ'
  def classSignature(typ : Type) : String = {
    typ match {
      case null => ""
      case typ : Class[_] =>
        "  "+typ.getCanonicalName() + "\n" +
         (for (i <- typ.getDeclaredFields().toList.sortBy(_.toString)) yield {
          "   "+i+"\n"
         }).mkString("") +
         classSignature(typ.getGenericSuperclass())
      case _ => ""
    }
  }

  override def writeClass(output : Output, t : Class[_]) : Registration = {
    val r = super.writeClass(output, t)

    if (r == null || seenClasses.contains(r.getType)) {
      output.writeString(null)
    } else {
      seenClasses += r.getType
      output.writeString(classSignature(r.getType))
    }

    r
  }

  override def readClass(input : Input) : Registration = {
    val r = super.readClass(input)

    val found = input.readString()

    if (r == null) { return null }
    if (found != null) {
      val expected = classSignature(r.getType)

      if (expected != found) {
        throw new IOException("Differing Jaam class signatures\n Expected:\n%s Found:\n%s".format(expected, found))
      }
    }

    r
  }

  // Serializer for InsnList that avoids stack overflows due to recursively
  // following AbstractInsnNode.next.  This works on concert with
  // AbstractInsnNodeSerializer.
  class InsnListSerializer() extends Serializer[InsnList] {
    override def write(kryo : Kryo, output : Output, collection : InsnList) {
      output.writeVarInt(collection.size(), true)
      for (element <- collection.iterator)
        kryo.writeClassAndObject(output, element);
    }

    override def read(kryo : Kryo, input : Input, typ : Class[InsnList]) : InsnList = {
      val collection = new InsnList()
      kryo.reference(collection)
      val length = input.readVarInt(true)
      for (i <- Seq.range(0, length))
        collection.add(kryo.readClassAndObject(input).asInstanceOf[AbstractInsnNode])
      return collection
    }
  }

  override def newDefaultSerializer(typ : Class[_]) : Serializer[_] = {
    if (classOf[InsnList] == typ)
      // We can't use addDefaultSerializer due to shading in the assembly
      new InsnListSerializer()
    else if (classOf[AbstractInsnNode].isAssignableFrom(typ)) {
      // Subclasses of AbstractInsnNode should not try to serialize prev or
      // next. However, this requires working around a bug in
      // rebuildCachedFields. (See AbstractInsnNodeSerializer.)
      val s = new AbstractInsnNodeSerializer(this, typ)
      s.removeField("prev")
      s.removeField("next")
      s
    } else {
      super.newDefaultSerializer(typ)
    }
  }

  // FieldSerializer.rebuildCachedFields has bugs relating to removed fields.
  // The following implementation works around these
  class AbstractInsnNodeSerializer(kryo : Kryo, typ : Class[_])
      extends FieldSerializer(this, typ) {
    override def rebuildCachedFields(minorRebuild : Boolean) {
      // Save and clear removedFields since the below calls to removeField
      // will repopulate it.  Otherwise, we get a ConcurentModificationException.
      val removed = this.removedFields
      if (!minorRebuild)
        this.removedFields = new java.util.HashSet()
      super.rebuildCachedFields(minorRebuild)
      if (!minorRebuild)
        for (field <- removed)
          // Make sure to use toString otherwise you call a version of
          // removeField that uses pointer equality and thus no effect
          removeField(field.toString)
    }
  }
}
