package org.ucombinator.jaam.tools

import java.io.FileInputStream
import scala.collection.mutable

import soot.{SootMethod => RealSootMethod} // TODO: fix class name collision
import soot.jimple.{Stmt => SootStmt, _}

import org.ucombinator.jaam.serializer._

object MissingReturns {
  def missingReturns(jaamFile : String) = {
    var states = List[State]()
    val stateSet = mutable.Set[(RealSootMethod, Int, String, String)]()

    val stream = new FileInputStream(jaamFile)
    val pi = new PacketInput(stream)
    var packet: Packet = null
    while ({packet = pi.read(); !packet.isInstanceOf[EOF]}) {
      packet match {
        case s@State(id, stmt, framePointer, kontStack) =>
          states +:= s
          stateSet.add((stmt.method, stmt.index, framePointer, kontStack))
        case _ => {}
      }
    }
    pi.close()

    for (State(id, stmt, framePointer, kontStack) <- states) {
      val isCall = stmt.stmt match {
        case sootStmt : InvokeStmt => true
        case sootStmt : DefinitionStmt =>
          sootStmt.getRightOp() match {
            case rhs : InvokeExpr => true
            case _ => false
          }
        case _ => false
      }

      // We can't use the next stmt, b/c Soot will complain that there is not
      // active body present.  Fortunately, we can use the stmt index instead.
      //val nextStmt = stmt.method.getActiveBody().getUnits().getSuccOf(stmt.stmt).asInstanceOf[SootStmt]
      if (isCall && !stateSet.contains((stmt.method, stmt.index + 1, framePointer, kontStack))) {
        println("Missing return for call in state " + id)
        println("  method: " + stmt.method)
        println("  index: " + stmt.index)
        println("  stmt: " + stmt.stmt)
        println("  fp: " + framePointer)
        println("  kontStack: " + kontStack)
      }
    }
  }
}
