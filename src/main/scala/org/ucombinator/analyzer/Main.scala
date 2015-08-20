package org.ucombinator.analyzer

/*
  Warning:

  1. Don't cross the streams.  It would be bad.

  2. Don't order addresses.  Dogs and cats, living together.  Mass hysteria.
*/

// TODO: need to track exceptions that derefing a Null could cause
// TODO: invoke main method instead of jumping to first instr so static init is done correctly
// TODO: invoke main method so we can initialize the parameters to main

import scala.collection.JavaConversions._
import scala.language.postfixOps
import xml.Utility

// We expect every Unit we use to be a soot.jimple.Stmt, but the APIs
// are built around using Unit so we stick with that.  (We may want to
// fix this when we build the Scala wrapper for Soot.)
import soot._
import soot.{Main => SootMain, Unit => SootUnit, Value => SootValue, Type => SootType}

import soot.shimple._

import soot.jimple._
import soot.jimple.{Stmt => SootStmt}

import soot.jimple.internal.JStaticInvokeExpr
import soot.jimple.internal.JArrayRef

import soot.tagkit._

import soot.util.Chain
import soot.options.Options
import soot.jimple.toolkits.invoke.AccessManager

//ICFG
import soot.toolkits.graph._
import soot.jimple.toolkits.ide.icfg._

// JGraphX

import javax.swing.JFrame
import javax.swing.SwingUtilities
import javax.swing.ToolTipManager

import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph
import com.mxgraph.layout.mxCompactTreeLayout

// Possibly thrown during transition between states.
case class UninitializedClassException(sootClass : SootClass) extends RuntimeException
case class UndefinedAddrsException(addrs : Set[Addr]) extends RuntimeException

// FramePointers, when paired with variable names, yield the addresses of variables.
abstract class FramePointer
case object InvariantFramePointer extends FramePointer
case class OneCFAFramePointer(val method : SootMethod, val nextStmt : Stmt) extends FramePointer
case object InitialFramePointer extends FramePointer

// BasePointers, when paired with field names, yield the addresses of fields.
abstract class BasePointer
case class OneCFABasePointer(stmt : Stmt, fp : FramePointer) extends BasePointer
case object InitialBasePointer extends BasePointer
// Note that due to interning, strings and classes may share base pointers with each other
// Oh, and class loaders are a headache(!)
case class StringBasePointer(val string : String) extends BasePointer
case class ClassBasePointer(val name : String) extends BasePointer
case class SnowflakeBasePointer(val clas : String) extends BasePointer

// Addresses of continuations on the stack
abstract class KontAddr
case class OneCFAKontAddr(val fr : FramePointer) extends KontAddr

// A continuation store paired with a continuation
case class KontStack(store : KontStore, k : Kont) {
  // TODO/generalize: Add widening in push and pop (to support PDCFA)

  // TODO/generalize: We need the control pointer of the target state to HANDLE PDCFA.
  def kalloca(frame : Frame) = OneCFAKontAddr(frame.fp)

  // TODO/generalize: Handle PDCFA
  def push(frame : Frame) : KontStack = {
    val kAddr = kalloca(frame)
    val newKontStore = store.update(kAddr, Set(k))
    KontStack(newKontStore, RetKont(frame, kAddr))
  }

  // Pop returns all possible frames beneath the current one.
  def pop() : Set[(Frame, KontStack)] = {
    k match {
      case RetKont(frame, kontAddr) => {
        for (topk <- store(kontAddr)) yield (frame, KontStack(store, topk))
        }
      case HaltKont => Set()
    }
  }

  def handleException(exception : Value,
                      stmt : Stmt,
                      fp : FramePointer,
                      store : Store,
                      initializedClasses : Set[SootClass]) : Set[State] = {
    if (!exception.isInstanceOf[ObjectValue]) throw new Exception("Impossible throw: stmt = " + stmt + "; value = " + exception)

    var visited = Set[(Stmt, FramePointer, KontStack)]()

    def f(stmt : Stmt, fp : FramePointer, kontStack : KontStack) : Set[State] = {
      if (visited.contains((stmt, fp, kontStack))) return Set()

      // TODO: Why does this give an error if we don't use "Tuple3"?
      visited = visited + Tuple3(stmt, fp, kontStack) // TODO: do we really need all of these in here?

      for (trap <- TrapManager.getTrapsAt(stmt.inst, stmt.method.getActiveBody())) {
        val caughtType = trap.getException()
        val newStore = store.update(CaughtExceptionFrameAddr(fp), D(Set(exception)))

        // TODO: use Hierarchy or FastHierarchy?
        if (State.isSubclass(exception.asInstanceOf[ObjectValue].sootClass, caughtType))
          return Set(State(stmt.copy(inst = trap.getHandlerUnit()), fp, newStore, this, initializedClasses))
      }

      (for ((frame, kontStack) <- this.pop()) yield { f(frame.stmt, frame.fp, kontStack) }).flatten
    }

    f(stmt, fp, this)

    // TODO: deal with unhandled exceptions
  }
}

// TODO Michael B: refactor KontStore and Store, since they only
// differ in their types
case class KontStore(private val map : Map[KontAddr, Set[Kont]]) {
  def update(addr : KontAddr, konts : Set[Kont]) : KontStore = {
    map.get(addr) match {
      case Some(oldd) => KontStore(map + (addr -> (oldd ++ konts)))
      case None => KontStore(map + (addr -> konts))
    }
  }

  def apply(addr : KontAddr) : Set[Kont] = map(addr)
  def get(addr : KontAddr) : Option[Set[Kont]] = map.get(addr)

  def prettyString() : List[String] =
    (for ((a, d) <- map) yield { a + " -> " + d }).toList
}


case class Frame(
  val stmt : Stmt,
  val fp : FramePointer,
  val destAddr : Option[Set[Addr]]) {

  def acceptsReturnValue() : Boolean = !(destAddr.isEmpty)
}

abstract class Kont

case class RetKont(
  val frame : Frame,
  val k : KontAddr
) extends Kont

object HaltKont extends Kont

case class D(val values: Set[Value]) {
  def join(otherd : D) : D = {
    D(values ++ otherd.values)
  }
  def maybeZero() : Boolean = values.exists(_.isInstanceOf[AtomicValue])
}

object D {
  val atomicTop = D(Set(AnyAtomicValue))
}

abstract class Value

abstract class AtomicValue extends Value

case object AnyAtomicValue extends AtomicValue

case class ObjectValue(val sootClass : SootClass,  val bp : BasePointer) extends Value

case class ArrayValue(val sootType : SootType, val bp : BasePointer) extends Value

abstract class Addr

abstract class FrameAddr extends Addr

case class LocalFrameAddr(val fp : FramePointer, val register : Local) extends FrameAddr

case class ParameterFrameAddr(val fp : FramePointer, val parameter : Int) extends FrameAddr

case class ThisFrameAddr(val fp : FramePointer) extends FrameAddr

case class CaughtExceptionFrameAddr(val fp : FramePointer) extends FrameAddr

case class InstanceFieldAddr(val bp : BasePointer, val sf : SootField) extends Addr

case class ArrayRefAddr(val bp : BasePointer) extends Addr

case class ArrayLengthAddr(val bp : BasePointer) extends Addr

case class StaticFieldAddr(val sf : SootField) extends Addr

case class Store(private val map : Map[Addr, D]) {
  def update(addr : Addr, d : D) : Store = {
    map.get(addr) match {
      case Some(oldd) => Store(map + (addr -> oldd.join(d)))
      case None => Store(map + (addr -> d))
    }
  }

  def update(addrs : Set[Addr], d : D) : Store = {
     var newStore = this
     for (a <- addrs) {
       newStore = newStore.update(a, d)
     }
     newStore
  }

  def update(m : Map[Addr, D]) : Store = {
    var newStore = this
    for ((a, d) <- m) {
      newStore = newStore.update(a, d)
    }
    newStore
  }

  def apply(addrs : Set[Addr]) : D = {
    val ds = for (a <- addrs; if map.contains(a)) yield map(a)
    val res = ds.fold (D(Set()))(_ join _)
    if (res == D(Set())) throw UndefinedAddrsException(addrs)
    res
  }

  def prettyString() : List[String] =
    (for ((a, d) <- map) yield { a + " -> " + d }).toList
}

case class Stmt(val inst : SootUnit, val method : SootMethod, val classmap : Map[String, SootClass]) {
  assert(inst.isInstanceOf[SootStmt])
  def nextSyntactic() : Stmt = this.copy(inst = method.getActiveBody().getUnits().getSuccOf(inst))
  override def toString() : String = inst.toString()
}

// State abstracts a collection of concrete states of execution.
case class State(stmt : Stmt,
                 fp : FramePointer,
                 store : Store,
                 kontStack : KontStack,
                 initializedClasses : Set[SootClass]) {

  // When you call next, you may realize you are going to end up throwing some exceptions
  // TODO/refactor: Consider turning this into a list of exceptions.
  var exceptions = D(Set())

  // Allocates a new frame pointer (currently uses 1CFA)
  def alloca(expr : InvokeExpr, nextStmt : Stmt) : FramePointer = OneCFAFramePointer(expr.getMethod(), nextStmt)
  // Allocates objects
  def malloc() : BasePointer = OneCFABasePointer(stmt, fp)

  // Any time you reference a class, you have to run this to make sure it's initialized.
  // If it isn't, the exception should be caught so the class can be initialized.
  def checkInitializedClasses(c : SootClass) {
    if (!initializedClasses.contains(c)) {
      throw new UninitializedClassException(c)
    }
  }

  // Returns all possible addresses of an assignable expression.
  // x = 3; // Should only return 1 address.
  // x[2] = 3; // May return more than one address because x might be multiple arrays.
  // x.y = 3; // May return multiple addresses ""
  // x[y] = 3; // May return multiple addresses even if x is a single value. (not currently relevant)
  // Right now, we model arrays as having only a single storage location, so the last example produces multiple addresses only based on there being more than one location for x.
  def addrsOf(lhs : SootValue) : Set[Addr] = {
    lhs match {
      case lhs : Local => Set(LocalFrameAddr(fp, lhs))
      case lhs : InstanceFieldRef => {
        val b : SootValue = lhs.getBase() // the x in x.y
        val d : D = eval(b)
        // TODO/optimize
        // filter out incorrect class types
        // TODO/bug
        // Suppose a number flows to x in x.y = 3;
        for (ObjectValue(_, bp) <- d.values)
         yield InstanceFieldAddr(bp, lhs.getField())
      }
      case lhs : StaticFieldRef => {
        val f : SootField = lhs.getField()
        val c : SootClass = f.getDeclaringClass()
        checkInitializedClasses(c)
        Set(StaticFieldAddr(f))
      }
      case lhs : ParameterRef => Set(ParameterFrameAddr(fp, lhs.getIndex()))
      case lhs : ThisRef => Set(ThisFrameAddr(fp))
        // TODO/precision: have multiple addresses per "catch" clause?
        // Perhaps mix left-hand side into address?
      case lhs : CaughtExceptionRef => Set(CaughtExceptionFrameAddr(fp))
      case lhs : ArrayRef => {
        // TODO/precision: Use more than a single address for each Array.
        val b = eval(lhs.getBase())
        // TODO/soundness: array ref out of bounds exception
        val i = eval(lhs.getIndex()) // Unused but evaled in case we trigger a <clinit> or exception
        // TODO: filter out incorrect types
        for (ArrayValue(_, bp) <- b.values) yield
          ArrayRefAddr(bp)
      }
    }
  }

  // Abstractly evaluate a Soot expression.
  // x.f
  // 3
  // a + b
  // We cheat slightly here, by using side-effects to install exceptions that need to be thrown.
  // This does not evaluate complex expressions like method-calls.
  def eval(v: SootValue) : D = {

    def assertNumeric(op : SootValue) {
      assert(op.getType().isInstanceOf[PrimType] && !op.getType().isInstanceOf[BooleanType])
    }
    def assertIntegral(op : SootValue) {
      assert(op.getType().isInstanceOf[PrimType] &&
        !op.getType().isInstanceOf[BooleanType] &&
        !op.getType().isInstanceOf[FloatType] &&
        !op.getType().isInstanceOf[DoubleType])
    }

    v match {
      //TODO CastExpr
      //TODO missing: CmplExpr, CmpgExpr, ConditionExpr, MethodHandle
      case (_ : Local) | (_ : Ref) => store(addrsOf(v))
      case _ : NullConstant => D.atomicTop
      case _ : NumericConstant => D.atomicTop
      case v : StringConstant => D(Set(ObjectValue(stmt.classmap("java.lang.String"), StringBasePointer(v.value))))
      case v : NegExpr => D.atomicTop
      case v : BinopExpr =>
        v match {
          case (_ : EqExpr) | (_ : NeExpr) | (_ : GeExpr) | (_ : GtExpr) | (_ : LeExpr) | (_ : LtExpr) =>
            assert(v.getOp1().getType.isInstanceOf[PrimType])
            assert(v.getOp2().getType.isInstanceOf[PrimType])
            eval(v.getOp1())
            eval(v.getOp2())
            D.atomicTop
          case (_ : ShrExpr) | (_ : ShlExpr) | (_ : UshrExpr) | (_ : RemExpr) | (_ : XorExpr) | (_ : OrExpr) | (_ : AndExpr) =>
            assertIntegral(v.getOp1())
            assertIntegral(v.getOp2())
            eval(v.getOp1())
            eval(v.getOp2())
            D.atomicTop
          case (_ : AddExpr) | (_ : SubExpr) | (_ : MulExpr) | (_ : DivExpr) =>
            assertNumeric(v.getOp1())
            assertNumeric(v.getOp2())
            eval(v.getOp1())
            val zCheck = eval(v.getOp2())
            if(v.isInstanceOf[DivExpr] && zCheck.maybeZero()){
              exceptions = exceptions.join(D(Set(ObjectValue(stmt.classmap("java.lang.ArithmeticException"), malloc()))))
            }
            D.atomicTop
        }

      // Every array has a distinguished field for its address.
      case v : LengthExpr => {
        val addrs : Set[Addr] = for (ArrayValue(_, bp) <- eval(v.getOp()).values) yield ArrayLengthAddr(bp)
        store(addrs)
      }


      // TODO/precision: implement the actual check
      case v : InstanceOfExpr => D.atomicTop

      // TODO/precision: take advantage of knowledge in v.getPreds()
      // (though we may be getting that knowledge from partial definedness of the store)
      // This code assumes that getValues returns only Local or Ref
      case v : PhiExpr => store((v.getValues map addrsOf).toSet.flatten)

      case _ =>  throw new Exception("No match for " + v.getClass + " : " + v)
    }
  }

  // The last parameter of handleInvoke allows us to override what
  // Stmt to execute after returning from this call.  We need this for
  // static class initialization because in that case we want to
  // return to the current statement instead of the next statement.
  def handleInvoke(expr : InvokeExpr,
                   destAddr : Option[Set[Addr]],
                   nextStmt : Stmt = stmt.nextSyntactic()) : Set[State] = {

    // o.f(3); // In this case, c is the type of o. m is f. receivers is the result of eval(o).
    // TODO/dragons. Here they be.
    def dispatch(c : SootClass, m : SootMethod, receivers : Set[Value]) : Set[State] = {
      // This function finds all methods that could override root_m.
      // These methods are returned with the root-most at the end of
      // the list and the leaf-most at the head.  Thus the caller
      // should use the head of the returned list.  The reason a list
      // is returned is so this function can recursively compute the
      // transitivity rule in Java's method override definition.
      //
      // Note that Hierarchy.resolveConcreteDispath should be able to do this, but seems to be implemented wrong
      def overloads(curr : SootClass, root_m : SootMethod) : List[SootMethod] = {
        val curr_m = curr.getMethodUnsafe(root_m.getName, root_m.getParameterTypes, root_m.getReturnType)
        if (curr_m == null) { overloads(curr.getSuperclass(), root_m) }
        else if (root_m.getDeclaringClass.isInterface || AccessManager.isAccessLegal(curr_m, root_m)) { List(curr_m) }
        else {
          val o = overloads(curr.getSuperclass(), root_m)
          (if (o.exists(m => AccessManager.isAccessLegal(curr_m, m))) List(curr_m) else List()) ++ o
        }
      }

      val meth = if (c == null) m else overloads(c, m).head
      // TODO: put a better message when there is no getActiveBody due to it being a native method
      val newFP = alloca(expr, nextStmt)
      var newStore = store
      for (i <- 0 until expr.getArgCount())
        newStore = newStore.update(ParameterFrameAddr(newFP, i), eval(expr.getArg(i)))
      val newKontStack = kontStack.push(Frame(nextStmt, fp, destAddr))
      // TODO/optimize: filter out incorrect class types
      val th = ThisFrameAddr(newFP)
      for (r <- receivers)
        newStore = newStore.update(th, D(Set(r)))

      Snowflakes.get(meth) match {
        case Some(h) => h(this, nextStmt, newFP, newStore, newKontStack)
        case None =>
          Set(State(Stmt(meth.getActiveBody().getUnits().getFirst, meth, stmt.classmap),
            newFP, newStore, newKontStack, initializedClasses))
      }
    }

    expr match {
      case expr : DynamicInvokeExpr => ??? // TODO: Could only come from non-Java sources
      case expr : StaticInvokeExpr =>
        checkInitializedClasses(expr.getMethod().getDeclaringClass())
        dispatch(null, expr.getMethod(), Set())
      case expr : InstanceInvokeExpr =>
        val d = eval(expr.getBase())
        val vs = d.values filter { case ObjectValue(sootClass, _) => State.isSubclass(sootClass, expr.getMethod().getDeclaringClass); case _ => false }
        ((for (ObjectValue(sootClass, _) <- vs) yield {
          val objectClass = if (expr.isInstanceOf[SpecialInvokeExpr]) null else sootClass
          dispatch(objectClass, expr.getMethod(), vs)
        }) :\ Set[State]())(_ ++ _) // TODO: better way to do this?
    }
  }

  // If you reference an unititialized field, what should it be?
  def defaultInitialValue(t : SootType) : D = {
    t match {
      case t : RefLikeType => eval(NullConstant.v())
      case t : PrimType => D.atomicTop // TODO/precision: should eval based on specific type
    }
  }

  // If you reference an unititialized static field, what should it be?
  def staticInitialValue(f : SootField) : D = {
    for (t <- f.getTags)
      t match {
        case t : DoubleConstantValueTag => return D.atomicTop
        case t : FloatConstantValueTag => return D.atomicTop
        case t : IntegerConstantValueTag => return D.atomicTop
        case t : LongConstantValueTag => return D.atomicTop
        case t : StringConstantValueTag => return D(Set(ObjectValue(stmt.classmap("java.lang.String"), StringBasePointer(t.getStringValue()))))
        case _ => ()
      }
    return defaultInitialValue(f.getType)
  }

  // Returns a Store containing the possibly nested arrays described
  // by the SootType t with dimension sizes 'sizes'
  def createArray(t : SootType, sizes : List[D], addrs : Set[Addr]) : Store = sizes match {
    // Should only happen on recursive calls. (createArray should never be called by a user with an empty list of sizes).
    case Nil => store.update(addrs, defaultInitialValue(t))
    case (s :: ss) => {
      val bp : BasePointer = malloc()
      // TODO/soundness: exception for a negative length
      // TODO/precision: stop allocating if a zero length
      // TODO/precision: separately allocate each array element
      createArray(t.asInstanceOf[ArrayType].getElementType(), ss, Set(ArrayRefAddr(bp)))
        .update(addrs, D(Set(ArrayValue(t, bp))))
        .update(ArrayLengthAddr(bp), s)
    }
  }

  // Returns the set of successor states to this state.
  def next() : Set[State] = {
    try {
      val nexts = true_next()
      val exceptionStates = (exceptions.values map { kontStack.handleException(_, stmt, fp, store, initializedClasses) }).flatten
      nexts ++ exceptionStates
    } catch {
      case UninitializedClassException(sootClass) =>
        // TODO/soundness: needs to also initialize parent classes
        exceptions = D(Set())
        val meth = sootClass.getMethodByNameUnsafe("<clinit>")
        if (meth != null) {
          // Initialize all static fields per JVM 5.4.2 and 5.5
          var newStore = store.update((for (f <- sootClass.getFields(); if f.isStatic) yield (StaticFieldAddr(f) -> staticInitialValue(f))).toMap)
          this.copy(store = newStore, initializedClasses = initializedClasses + sootClass)
            .handleInvoke(new JStaticInvokeExpr(meth.makeRef(), java.util.Collections.emptyList()), None, stmt)
        } else {
          Set(this.copy(initializedClasses = initializedClasses + sootClass))
        }

      case UndefinedAddrsException(addrs) => {
        println()
        println(Console.RED + "!!!!! ERROR: Undefined Addrs !!!!!")
        println("!!!!! stmt = " + stmt + " !!!!!")
        println("!!!!! addrs = " + addrs + " !!!!!" + Console.RESET)
        Set()
      }
    }
  }

  def true_next() : Set[State] = {
    stmt.inst match {
      case inst : InvokeStmt => handleInvoke(inst.getInvokeExpr, None)

      case inst : DefinitionStmt => {
        val lhsAddr = addrsOf(inst.getLeftOp())

        inst.getRightOp() match {
          case rhs : InvokeExpr => handleInvoke(rhs, Some(lhsAddr))
          case rhs : NewExpr => {
            val baseType : RefType = rhs.getBaseType()
            val sootClass = baseType.getSootClass()
            val bp : BasePointer = malloc()
            val obj : Value = ObjectValue(sootClass, bp)
            val d = D(Set(obj))
            var newStore = store.update(lhsAddr, d)
            checkInitializedClasses(sootClass)
            // initialize instance fields to default values for their type
            def initInstanceFields(c : SootClass) {
              for (f <- c.getFields) {
                newStore = newStore.update(InstanceFieldAddr(bp, f), defaultInitialValue(f.getType))
              }

              if(c.hasSuperclass) initInstanceFields(c.getSuperclass)
            }
            initInstanceFields(sootClass)
            Set(this.copy(stmt = stmt.nextSyntactic(), store = newStore))
          }
          // TODO/clarity: Add an example of Java code that can trigger this.
          case rhs : ClassConstant => { // TODO: frustratingly similar to NewExpr
            val newStore = store.update(lhsAddr, D(Set(ObjectValue(stmt.classmap("java.lang.Class"), ClassBasePointer(rhs.value)))))
            Set(this.copy(stmt = stmt.nextSyntactic(), store = newStore))
          }
          case rhs : NewArrayExpr => {
            // Value of lhsAddr will be set to a pointer to the array. (as opposed to the array itself)
            val newStore = createArray(rhs.getType(), List(eval(rhs.getSize())), lhsAddr)
            Set(this.copy(stmt = stmt.nextSyntactic(), store = newStore))
          }
          case rhs : NewMultiArrayExpr => {
            //see comment above about lhs addr
            val newStore = createArray(rhs.getType(), rhs.getSizes().toList map eval, lhsAddr)
            Set(this.copy(stmt = stmt.nextSyntactic(), store = newStore))
          }
          case rhs => {
            val newStore = store.update(lhsAddr, eval(rhs))
            Set(this.copy(stmt = stmt.nextSyntactic(), store = newStore))
          }
        }
      }

      case inst : IfStmt => {
            eval(inst.getCondition()) //in case of side effects //TODO/precision evaluate the condition
            val trueState = this.copy(stmt = stmt.copy(inst = inst.getTarget()))
            val falseState = this.copy(stmt = stmt.nextSyntactic())
        Set(trueState, falseState)
      }

      case inst : SwitchStmt =>
        //TODO/prrecision dont take all the switches
        inst.getTargets().map(t => this.copy(stmt = stmt.copy(inst = t))).toSet

      case inst : ReturnStmt => {
        val evaled = eval(inst.getOp())
        for ((frame, newStack) <- kontStack.pop) yield {
          val newStore = if (frame.acceptsReturnValue()) {
            store.update(frame.destAddr.get, evaled)
          } else {
            store
          }

          State(frame.stmt, frame.fp, newStore, newStack, initializedClasses)
        }
      }

      case inst : ReturnVoidStmt => {
        for ((frame, newStack) <- kontStack.pop if !(frame.acceptsReturnValue())) yield
          State(frame.stmt, frame.fp, store, newStack, initializedClasses)
      }

      // Since Soot's NopEliminator run before us, no "nop" should be
      // left in the code and this case isn't needed (and also is
      // untested).  The one place a "nop" could occur is as the last
      // instruction of a method that is also the instruction after
      // the end of a "try" clause. (See NopEliminator for the exact
      // conditions.) However, that would not be an executable
      // instruction, so we still wouldn't need this case.
      //
      // If we ever need the code for this, it would probably be:
      //   Set(State(stmt.nextSyntactic(), fp, store, kontStack, initializedClasses))
      case inst : NopStmt => throw new Exception("Impossible statement: " + inst)

      case inst : GotoStmt => Set(this.copy(stmt = stmt.copy(inst = inst.getTarget())))

      // For now we don't model monitor statements, so we just skip over them
      // TODO/soundness: In the event of multi-threaded code with precise interleaving, this is not sound.
      case inst : EnterMonitorStmt => Set(this.copy(stmt = stmt.nextSyntactic()))
      case inst : ExitMonitorStmt => Set(this.copy(stmt = stmt.nextSyntactic()))

      // TODO: needs testing
      case inst : ThrowStmt => { exceptions = exceptions.join(eval(inst.getOp())); Set() }

      // TODO: We're missing BreakPointStmt and RetStmt (but these might not be used)

      case _ => {
        throw new Exception("No match for " + stmt.inst.getClass + " : " + stmt.inst)
      }
    }
  }
}

// TODO/refactor: Maybe allow user-specified arguments.
object State {
  val initialFramePointer = InitialFramePointer
  val initialBasePointer = InitialBasePointer
  def inject(stmt : Stmt) : State = {
    val stringClass : SootClass = stmt.classmap("java.lang.String")
    val initial_map : Map[Addr, D] = Map(
      (ParameterFrameAddr(initialFramePointer, 0) -> D(Set(ArrayValue(stringClass.getType(), initialBasePointer)))),
      (ArrayRefAddr(initialBasePointer) -> D(Set(ObjectValue(stringClass, initialBasePointer)))),
      (ArrayLengthAddr(initialBasePointer) -> D.atomicTop))
    State(stmt, initialFramePointer, Store(initial_map), KontStack(KontStore(Map()), HaltKont), Set())
  }

  def isSubclass(sub : SootClass, sup : SootClass) : Boolean =
    Scene.v().getActiveHierarchy().isClassSubclassOfIncluding(sub, sup)
}

// Uniquely identifies a particular method somewhere in the program.
case class MethodDescription(val className : String,
                             val methodName : String,
                             val parameterTypes : List[String],
                             val returnType : String)

// Snowflakes are special-cased methods
abstract class SnowflakeHandler {
  def apply(state : State,
            nextStmt : Stmt,
            newFP : FramePointer,
            newStore : Store,
            newKontStack : KontStack) : Set[State]
}

object Snowflakes {
  val table = scala.collection.mutable.Map.empty[MethodDescription, SnowflakeHandler]
  def get(meth : SootMethod) : Option[SnowflakeHandler] =
    table.get(MethodDescription(
      meth.getDeclaringClass.getName,
      meth.getName,
      meth.getParameterTypes.toList.map(_.toString()),
      meth.getReturnType.toString()))
  def put(md : MethodDescription, handler : SnowflakeHandler) { table.put(md, handler) }
}

object NoOpSnowflake extends SnowflakeHandler {
  override def apply(state : State,
                     nextStmt : Stmt,
                     newFP : FramePointer,
                     newStore : Store,
                     newKontStack : KontStack) : Set[State] =
    Set(state.copy(stmt = nextStmt))
}

// TODO/soundness: Add JohnSnowflake for black-holes. Not everything becomes top, but an awful lot will.

case class ConstSnowflake(value : D) extends SnowflakeHandler {
  override def apply(state : State, nextStmt : Stmt, newFP : FramePointer, newStore : Store, newKontStack : KontStack) = {
    val newNewStore = state.stmt.inst match {
      case inst : DefinitionStmt => state.store.update(state.addrsOf(inst.getLeftOp()), value)
      case inst : InvokeStmt => state.store
    }
    Set(state.copy(stmt = nextStmt, store = newNewStore))
  }
}

object Main {
  def main(args : Array[String]) {
    // TODO: proper option parsing
    if (args.length != 3) println("Expected arguments: [classDirectory] [className] [methodName]")
    val classDirectory = args(0)
    val className = args(1)
    val methodName = args(2)

    val classes : Map[String, SootClass] = getClassMap(getShimple(classDirectory, ""))

      // Snowflakes are special Java procedures whose behavior we know and special-case.
      // For example, native methods (that would be difficult to analyze) are snowflakes.
    Snowflakes.put(MethodDescription("java.io.PrintStream", "println", List("int"), "void"),
      new SnowflakeHandler {
        override def apply(state : State,
                     nextStmt : Stmt,
                     newFP : FramePointer,
                     newStore : Store,
                     newKontStack : KontStack) = {
          System.err.println("Skipping call to java.io.OutputStream.println(int) : void")
          Set(state.copy(stmt = nextStmt))
        }
      })
    Snowflakes.put(MethodDescription("java.lang.System","<clinit>", List(), "void"),
      new SnowflakeHandler {
        override def apply(state : State,
                     nextStmt : Stmt,
                     newFP : FramePointer,
                     newStore : Store,
                     newKontStack : KontStack) = {
          def updateStore(oldStore : Store, clas : String, field : String, typ : String) =
            oldStore.update(StaticFieldAddr(classes(clas).getFieldByName(field)),
              D(Set(ObjectValue(classes(typ),
                  SnowflakeBasePointer(clas + "." + field)))))
          var newNewStore = newStore
          newNewStore = updateStore(newNewStore,
              "java.lang.System", "in", "java.io.InputStream")
          newNewStore = updateStore(newNewStore,
              "java.lang.System", "out", "java.io.PrintStream")
          newNewStore = updateStore(newNewStore,
              "java.lang.System", "err", "java.io.PrintStream")
          newNewStore = updateStore(newNewStore,
              "java.lang.System", "security", "java.lang.SecurityManager")
          newNewStore = updateStore(newNewStore,
              "java.lang.System", "cons", "java.io.Console")

          Set(state.copy(stmt = nextStmt,
            store = newNewStore))
        }
      })
    Snowflakes.put(MethodDescription("java.lang.Class", "desiredAssertionStatus", List(), "boolean"), ConstSnowflake(D.atomicTop))
    Snowflakes.put(MethodDescription("java.lang.Throwable", "<init>", List(), "void"), NoOpSnowflake)
    Snowflakes.put(MethodDescription("java.lang.Throwable", "<clinit>", List(), "void"), NoOpSnowflake)
    Snowflakes.put(MethodDescription("java.util.ArrayList", "<init>", List("int"), "void"), NoOpSnowflake)

    val mainMainMethod : SootMethod = classes(className).getMethodByName(methodName);
    val insts : Chain[SootUnit] = mainMainMethod.getActiveBody().getUnits();

    val first : SootUnit = insts.getFirst()

      // Setting up the GUI
    val window = new Window
    val initialState = State.inject(Stmt(first, mainMainMethod, classes))
    window.addState(initialState)


    var todo : List [State] = List(initialState)
    var seen : Set [State] = Set()


      // Explore the state graph
    while (todo nonEmpty) {
      val current = todo.head
      println(current)
      println()
      val nexts : Set[State] = current.next()
      for (n <- nexts) window.addNext(current, n)

      // TODO: Fix optimization bug here
      todo = nexts.toList.filter(!seen.contains(_)) ++ todo.tail
      seen = seen ++ nexts

    }

    println("Done!")

  }

   def getClassMap(classes : Chain[SootClass]) : Map[String, SootClass] =
    (for (c <- classes) yield c.getName() -> c).toMap

  def getShimple(classesDir : String, classPath : String) = {
    Options.v().set_output_format(Options.output_format_shimple);
    Options.v().set_verbose(false);
    Options.v().set_include_all(true);
    // we need to link instructions to source line for display
    Options.v().set_keep_line_number(true);
    // Called methods without jar files or source are considered phantom
    Options.v().set_allow_phantom_refs(true);
    // Include the default classpath, which should include the Java SDK rt.jar.
    Options.v().set_prepend_classpath(true);
    Options.v().set_process_dir(List(classesDir));
    // Include the classesDir on the class path.
    Options.v().set_soot_classpath(classesDir + ":" + classPath);
    // Prefer definitions from class files over source files
    Options.v().set_src_prec(Options.src_prec_class);
    // Compute dependent options
    SootMain.v().autoSetOptions();
    // Load classes according to the configured options
    Scene.v().loadNecessaryClasses();
    // Run transformations and analyses according to the configured options.
    // Transformation could include jimple, shimple, and CFG generation
    PackManager.v().runPacks();
    Scene.v().getApplicationClasses();
  }
}

class Window extends JFrame ("Shimple Analyzer") {
  // TODO: make exiting window go back to repl
  // TODO: save graph files to review later
  val graph = new mxGraph() {
    override def getToolTipForCell(cell : Object) : String = {
      vertexToState.get(cell) match {
        case None => super.getToolTipForCell(cell)
        case Some(state) =>
          var tip = "<html>"
          tip += "FP: " + Utility.escape(state.fp.toString) + "<br><br>"
          tip += "Kont: " + Utility.escape(state.kontStack.k.toString) + "<br><br>"
          tip += "Store:<br>" + state.store.prettyString.foldLeft("")(_ + "&nbsp;&nbsp;" + Utility.escape(_) + "<br>") + "<br>"
          tip += "KontStore:<br>" + state.kontStack.store.prettyString.foldLeft("")(_ + "&nbsp;&nbsp;" + Utility.escape(_) + "<br>")
          //tip += "InitializedClasses: " + state.initializedClasses.toString + "<br>"
          tip += "</html>"
          tip
      }
    }
  }


  private val layoutX = new mxCompactTreeLayout(graph, false)
  private val parentX = graph.getDefaultParent()
  private val graphComponent = new mxGraphComponent(graph)
  private var stateToVertex = Map[State,Object]()
  private var vertexToState = Map[Object,State]()


  graphComponent.setEnabled(false)
  graphComponent.setToolTips(true)
  getContentPane().add(graphComponent)
  ToolTipManager.sharedInstance().setInitialDelay(0)
  ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE)
  setSize(400, 320)
  setExtendedState(java.awt.Frame.MAXIMIZED_BOTH)
  setVisible(true)

  private def stateString(state : State) : String = state.stmt.method.toString() + "\n" + state.stmt.inst.toString()

  def addState(state : State) {
    val vertex = graph.insertVertex(parentX, null, stateString(state), 100, 100, 20, 20, "ROUNDED")
    graph.updateCellSize(vertex)
    stateToVertex += (state -> vertex)
    vertexToState += (vertex -> state)
  }

  def addNext(start : State, end : State) {
    graph.getModel().beginUpdate()
    try {
      stateToVertex.get(end) match {
        case None =>
          val tag = stateString(end)
          val v = graph.insertVertex(parentX, null, tag, 240, 150, 80, 30, "ROUNDED")
          graph.updateCellSize(v)
          graph.insertEdge(parentX, null, null, stateToVertex(start), v)
          stateToVertex += (end -> v)
          vertexToState += (v -> end)
        case Some(v) =>
          graph.insertEdge(parentX, null, null, stateToVertex(start), v)
      }
      // TODO: layout basic blocks together
      layoutX.execute(parentX)
    }
    finally
    {
      graph.getModel().endUpdate()
    }
  }
}

//future rendering of control flow graphs
class CFG extends JFrame ("Control Flow Graph") {

  val graph = new mxGraph()

  private val layoutX = new mxCompactTreeLayout(graph, false)
  private val parentX = graph.getDefaultParent()
  private val graphComponent = new mxGraphComponent(graph)
  private var stateToVertex = Map[Block,Object]()
  private var vertexToState = Map[Object,Block]()


  graphComponent.setEnabled(false)
  graphComponent.setToolTips(true)
  getContentPane().add(graphComponent)
  ToolTipManager.sharedInstance().setInitialDelay(0)
  ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE)
  setSize(400, 320)
  setExtendedState(java.awt.Frame.MAXIMIZED_BOTH)
  setVisible(true)


   def addState(inst : Block) {
    val vertex = graph.insertVertex(parentX, null, inst, 100, 100, 20, 20, "ROUNDED")
    graph.updateCellSize(vertex)
    stateToVertex += (inst -> vertex)
    vertexToState += (vertex -> inst)
  }

   def addNext(start : Block, end : Block) {
    graph.getModel().beginUpdate()
    try {
      stateToVertex.get(end) match {
        case None =>
          //val tag = stateString(end)
          val v = graph.insertVertex(parentX, null, end, 240, 150, 80, 30, "ROUNDED")
          graph.updateCellSize(v)
          graph.insertEdge(parentX, null, null, stateToVertex(start), v)
          stateToVertex += (end -> v)
          vertexToState += (v -> end)
        case Some(v) =>
          graph.insertEdge(parentX, null, null, stateToVertex(start), v)
      }
      layoutX.execute(parentX)
    }
    finally
    {
      graph.getModel().endUpdate()
    }
  }

}

