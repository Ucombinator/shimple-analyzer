package org.ucombinator.jaam.tools

import java.io.FileInputStream
import scala.collection.mutable

import org.ucombinator.jaam.serializer._

object Print {
  def printFile(jaamFile : String) = {
    val stream = new FileInputStream(jaamFile)
    val pi = new PacketInput(stream)
    var packet: Packet = null
    while ({packet = pi.read(); !packet.isInstanceOf[EOF]}) {
      packet match {
        case p : Node => printNode(p)
        case p : Edge => printEdge(p)
        case p : NodeTag => printTag(p)
      }
    }
    pi.close()
  }

  object FoundNode extends Exception {}

  def printNodeFromFile(jaamFile : String, nodeIDVal : Int) = {
    val nodeID = Id[Node](nodeIDVal)
    val stream = new FileInputStream(jaamFile)
    val pi = new PacketInput(stream)
    var packet: Packet = null
    try {
      while ({packet = pi.read(); !packet.isInstanceOf[EOF]}) {
        packet match {
          case p: Node => if (p.id == nodeID) {
            throw FoundNode
          }
          case _ => ()
        }
      }
    } catch {
      case FoundNode => packet match {
        case p : Node => printNode(p)
      }
    }
    pi.close()
  }

  def printNode(node : Node) = {
    node match {
      case n: State => printState(n)
      case n: ErrorState => printErrorState(n)
    }
  }

  def printState(state : State) = {
    val identifier = "node-" + state.id.id
    printIndentedLine(identifier, 0, "State")
    printIndentedLine(identifier, 1, "id: " + state.id.id)
    printIndentedLine(identifier, 1, "index: " + state.stmt.index)
    printIndentedLine(identifier, 1, "sootMethod: " + state.stmt.method.toString)
    printIndentedLine(identifier, 2, "returns: " + state.stmt.method.getReturnType.toString)
    if (state.stmt.method.getParameterCount == 0) {
      printIndentedLine(identifier, 2, "takes: <none>")
    } else {
      printIndentedLine(identifier, 2, "takes: " + state.stmt.method.getParameterTypes.toArray.mkString(", "))
    }
    printIndentedLine(identifier, 1, "sootStmt: " + state.stmt.stmt.toString())
    printIndentedLine(identifier, 1, "framePointer: " + state.framePointer)
    printIndentedLine(identifier, 1, "kontStack: " + state.kontStack)
  }

  def printErrorState(errorState : ErrorState) = {
    val identifier = "node-" + errorState.id.id
    printIndentedLine(identifier, 0, "ErrorState")
    printIndentedLine(identifier, 1, "id: " + errorState.id.id)
  }

  def printEdge(edge : Edge) = {
    val identifier = "edge-" + edge.id.id
    printIndentedLine(identifier, 0, "Edge")
    printIndentedLine(identifier, 1, "id: " + edge.id.id)
    printIndentedLine(identifier, 1, "from: " + edge.src.id)
    printIndentedLine(identifier, 1, "to: " + edge.dst.id)
  }

  def printTag(tag : NodeTag) = {
    tag.tag match {
      case _ : AllocationTag => printAllocationTag(tag)
      case _ => ()
    }
  }

  def printAllocationTag(tag : NodeTag) = {
    val identifier = "tag-" + tag.id.id
    printIndentedLine(identifier, 0, "AllocationTag")
    printIndentedLine(identifier, 1, "id: " + tag.id.id)
    printIndentedLine(identifier, 1, "node: " + tag.node.id)
  }

  def printIndentedLine(identifier : String, indent : Int, s : String = "") = {
    println(identifier + (if (s == "") "" else " " + ("    " * indent) + s))
  }
}
