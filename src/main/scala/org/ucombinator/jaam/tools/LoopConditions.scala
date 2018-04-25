package org.ucombinator.jaam.tools.loopConditions

import java.io.{FileOutputStream, PrintStream}

import org.jgrapht.Graphs
import org.jgrapht.graph.{AsSubgraph, DefaultEdge, EdgeReversedGraph}
import org.jgrapht.traverse.TopologicalOrderIterator
import org.ucombinator.jaam.serializer.Serializer
import org.ucombinator.jaam.tools
import org.ucombinator.jaam.tools.app.{App, Origin}
import org.ucombinator.jaam.util.{JGraphT, Soot, Stmt}
import soot.jimple.{Stmt => SootStmt, _}
import soot.options.Options
import soot.{Main => SootMain, Unit => SootUnit, Value => SootValue, _}

import scala.collection.JavaConverters._
import scala.collection.immutable

object Main {
  def main(input: List[String], className: String) {
    Options.v().set_verbose(false)
    Options.v().set_output_format(Options.output_format_jimple)
    Options.v().set_keep_line_number(true)
    Options.v().set_allow_phantom_refs(true)
    //Options.v().set_soot_classpath(classpath.mkString(":"))
    Options.v().set_include_all(true)
    Options.v().set_prepend_classpath(false)
    Options.v().set_src_prec(Options.src_prec_only_class)
    //Options.v().set_whole_program(true)-
    //Options.v().set_app(true)-
    soot.Main.v().autoSetOptions()

    //Options.v().setPhaseOption("cg", "verbose:true")
    //Options.v().setPhaseOption("cg.cha", "enabled:true")

    //Options.v.set_main_class(mainClass)
    //Scene.v.setMainClass(clazz)
    //Scene.v.addBasicClass(className, SootClass.HIERARCHY)
    //Scene.v.setSootClassPath(classpath)
    //Scene.v.loadNecessaryClasses

    Soot.useJaamClassProvider()

    val inputPackets = input.flatMap(Serializer.readAll(_).asScala)

    for (a <- inputPackets) { Soot.addClasses(a.asInstanceOf[App]) }

/*
    val mainClasses = for (a <- inputPackets) yield { a.asInstanceOf[App].main.className }
    val mainMethods = for (a <- inputPackets) yield { a.asInstanceOf[App].main.methodName }
    val mainClass = mainClasses.head.get // TODO: fix
    val mainMethod = mainMethods.head.get // TODO: fix
    */

    Scene.v.loadBasicClasses()
    PackManager.v.runPacks()

    // Get all classes loaded so Soot doesn't keep recomputing the Hierarchy
    for (name <- Soot.loadedClasses.keys) {
      Soot.getSootClass(name)
      //println(f"class name: $name")
    }

    for (name <- Soot.loadedClasses.keys) {
      if (Soot.loadedClasses(name).origin == Origin.APP) {
        for (m <- Soot.getSootClass(name).getMethods.asScala) {
          if (!m.isNative && !m.isAbstract) {
            //    .getMethodByName(mainMethod) //Coverage2.freshenMethod(Soot.getSootClass(mainClass).getMethodByName(mainMethod))
            //println(f"\n\n!!!!!!!!\nMethod: $name.${m.getName}\n!!!!!!!!\n\n")
            myLoops(m)
          }
        }
      }
    }
  }

  // TEST CMD: (cd ../..; sbt assembly) && jaam loop4 --input DoWhileLoop.app.jaam --output /dev/null
  def myLoops(m: SootMethod): Unit = {
// TODO: replace set with ordered set?
    val (start, graph) = Soot.getBodyGraph(m)



    /*println(f"start: ${start.index}: $start\n")
    if (true) {
    println(f"graph:\n")
    for (v <- graph.vertexSet.asScala.toList.sortBy(_.index)) {
      println(f"  vertex: ${v.index}: $v")
      for (p <- Graphs.successorListOf(graph, v).asScala.toList.sortBy(_.index)) {
        println(f"    ${p.index}: $p")
      }
    }
    println()
    }*/

    val dom = JGraphT.dominators(graph, start, true)

    if (false) {
      println(f"dom:\n")
      for ((k, vs) <- dom.toList.sortBy(_._1.index)) {
        println(f"  key: ${k.index}: $k")
        for (v <- vs.toList.sortBy(_.index)) {
          println(f"    ${v.index}: $v")
        }
      }
      println()
    }

    // Maps header nodes to sets of backjump nodes
    val headers = JGraphT.loopHeads(graph, start)

    val loops = JGraphT.loopNodes(graph, start)
    if (loops.size > 0) {

      /*println(f"headers:\n")
      for ((k, vs) <- headers.toList.sortBy(_._1.index)) {
        println(f"  key: $k")
        for (v <- vs.toList.sortBy(_.index)) {
          println(f"    $v")
        }
      }
      println()*/

      // Print Method's name and Class's name
      println(f"\n\n*******\n*******\nMethod: ${m.getDeclaringClass.getName}.${m.getName}\n\n")

      println(f"Loops:\n")
      for ((k, vs) <- loops.toList.sortBy(_._1.index)) {
        println(f"-------\nHeader:\n$k\nVertices:\n")
        for (v <- vs.toList.sortBy(_.index)) {
          println(f"    $v")
          for (p <- Graphs.successorListOf(graph, v).asScala.toList.sortBy(_.index)) {
            println(f"    ${p.index}: $p")
          }
        }
      }
      println()
    }

    for ((k, vs) <- loops.toList.sortBy(_._1.index)) {
      try {
        val loopGraph = new AsSubgraph[Stmt, DefaultEdge](graph, vs.asJava)
        //println(f"loop at $k")

        // Nodes one past the end
        val ends = vs.flatMap(v => Graphs.successorListOf(graph, v).asScala.filter(s => !vs.contains(s)))
        //println(f"  ends $ends")
        val backEdges = headers(k)

        // c = start of condition
        // s = start of body
        // t = end of body
        // e = first statement after loop

        // Types of loops: exception, infinite, pre-condition, post-condition
        if (k match {
          case k: IdentityStmt => k.getRightOp.isInstanceOf[CaughtExceptionRef]
          case _ => false
        }) {
          println(f"Loop type: exception (by identity)")
        } else if (k match {
          case k: DefinitionStmt => k.getRightOp.isInstanceOf[CaughtExceptionRef]
          case _ => false
        }) {
          println(f"Loop type: exception (by definition)")
        } else if (vs.forall(v => v.nextSemantic.forall(vs))) {
          // infinite = no jumps out of loop
          println(f"Loop type: infinite")

        } else if (backEdges.forall(b => b.nextSemantic.forall(vs))) {
          // pre-condition = jumps out of loop - back jump statements go only into the loop
          println(f"Loop type: pre-condition")

          val c: Stmt = k

          // Note, there may be multiple `ends` due to a "return" inside the loop.
          // However, the "real" e is the one that has a set of predecessors that all have paths to the predecessors of all the other members of `ends`.
          // Thus we can partially order the predecessors of the members of `ends` and take the first one.
          // This may not be totally ordered but the set of initial statements in the partial order all go to the same e.

          //println(f"  ends.size > 1")
          // Remove edges going to c
          Graphs.predecessorListOf(loopGraph, c).asScala.foreach(loopGraph.removeEdge(_, c))

          // Create a topological traversal
          val i = new TopologicalOrderIterator[Stmt, DefaultEdge](loopGraph)

          // Filter i to contain only those that have some successor that is not in the loop
          val filtered = i.asScala.filter(v => Graphs.successorListOf(graph, v).asScala.exists(!vs(_)))

          // Take the first one.  This jumps to the "real" `e`
          val p = filtered.next
          /*println(f"  p = $p")
          println(f"  ends = $ends")
          println(f"  p.succ = ${Graphs.successorListOf(graph, p).asScala}")*/

          // The member of `ends` that is preceded by `p` is the "real" `e`
          val es = Graphs.successorListOf(graph, p).asScala.filter(!vs(_))
          assert(es.size == 1) // There should be only one
          val e = es.toList.head
          assert(ends(e))

          // Condition = nodes between c and s
          // (1a) s = first node that is post-dominated by t (relative to e or c)
          // (1b) s = first node that dominates t (relative to e or c) (?)
          // Note: 1a is the same as 1b
          // (2) s = first choke point after c (?)
          // (3) s = statement after last edge to e (Generalize?)
          // (4a) Condition = everything reachable backwards from e but not through edge t->c
          // (4b) Condition = everything between c and e but not through edge t->c (?)
          // Note: 4a is the same as 4b
          // Note: (1) is the same as (4) (?)

          //println(f"  c = $c")
          //println(f"  e = $e")

          // Compute 4a
          val cs1 = {
            val init = Graphs.predecessorListOf(graph, e).asScala.filter(vs)
            var cond = init.toSet
            var todo = init.toList
            while (todo.nonEmpty) {
              val v = todo.head
              todo = todo.tail
              if (v != c) {
                //println(f"  v = $v")
                val predecessor = Graphs.predecessorListOf(graph, v).asScala.filterNot(cond)
                //println(f"  pred = $predecessor")
                todo ++= predecessor
                cond ++= predecessor
              }
            }

            /*println(f"  (1) condition = ")
            for (s <- cond.toList.sortBy(_.index)) {
              println(f"    $s")
            }*/

            cond
          }

          // Compute 2
          val cs2 = {
            // Create a topological traversal
            val i = new TopologicalOrderIterator[Stmt, DefaultEdge](loopGraph)

            var seen = Set[Stmt]() // Stmts that we have seen jumps to
            var visited = Set[Stmt]() // Stmts that the topological traversal has visited
            val ps = loopGraph.vertexSet().asScala.filter(_.nextSemantic.contains(e)) // all states that jump to e

            //println(f"  ps = $ps")

            var v: Stmt = i.next
            visited += v
            //println(f"  v.init = $v")
            while (!ps.subsetOf(visited) // not after all jumps to e
              || !seen.subsetOf(visited)) {
              // not at choke point
              seen ++= Graphs.successorListOf(loopGraph, v).asScala

              v = i.next
              visited += v
              /*
              println(f"  v = $v")
              println(f"  seen = $seen")
              println(f"  visited = $visited")*/
            }

            if (!v.nextSemantic.contains(e)) {
              visited -= v
            }

            /*println(f"  (2) condition = ")
            for (s <- visited.toList.sortBy(_.index)) {
              println(f"    $s")
            }*/

            visited
          }

          // Compute 3
          val cs3 = {

            // Create a topological traversal
            val i = new TopologicalOrderIterator[Stmt, DefaultEdge](loopGraph)

            // Filter i to contain only those that have some successor that is not in the loop
            val filtered = i.asScala.filter(_.nextSemantic.contains(e))

            // Set of nodes that jump to e
            var ps = loopGraph.vertexSet().asScala.filter(_.nextSemantic.contains(e))

            // Take the last condition.  This jumps to the "real" `s`
            while (ps.size > 1) {
              ps -= filtered.next
            }
            val lc = ps.head
            //println(f"  lc = $lc")
            //println(f"  lc.succ = ${Graphs.successorListOf(graph, lc).asScala}")

            // Finds 's'
            val ss = lc.nextSemantic.filter(_ != e)
            assert(ss.size == 1)
            val s = ss.head
            //println(f"  s = $s")

            // Traverse from c to s:
            var cond = Set(c)
            var todo = List(c)
            while (todo.nonEmpty) {
              val v = todo.head
              todo = todo.tail
              if (v != s) {
                //println(f"  v = $v")
                val successor = Graphs.successorListOf(graph, v).asScala.filterNot(cond).filter(vs)
                //println(f"  succ = $successor")
                todo ++= successor
                cond ++= successor
              }
            }
            cond -= s

            /*println(f"  (3) condition = ")
            for (s <- cond.toList.sortBy(_.index)) {
              println(f"    $s")
            }*/

            cond
          }

          if (cs1 != cs2) {
            println("!!!!!\n!!!!!\n!!!!!\nCase 1 not equal Case 2")
          }
          if (cs1 != cs3) {
            println("!!!!!\n!!!!!\n!!!!!\nCase 1 not equal Case 3")
          }
          if (cs1 == cs2 && cs1 == cs3) {
            println("Correct")
          }

        } else {
          // post-condition = some back jump statements go out of loop
          println(f"Loop type: post-condition")

          /*for (v <- vs) {
            println(f"  v: $v")
          }
          for (b <- backEdges) {
            println(f"  backedge: $b")
            println(f"  nextSemantic: ${b.nextSemantic}")
          }*/

          val s: Stmt = k

          // Note, there may be multiple `ends` due to a "return" inside the loop.
          // However, the "real" e is the one that has a set of predecessors that predecessors of all other members of 'ends' have paths to them.
          // Thus we can partially order the predecessors of the members of `ends` and take the last one.
          // This may not be totally ordered but the set of final statements in the partial order all go to the same e.

          //println(f"  ends.size > 1")

          // Remove edges going to s
          Graphs.predecessorListOf(loopGraph, s).asScala.foreach(loopGraph.removeEdge(_, s))

          // Create a topological traversal
          val i = new TopologicalOrderIterator[Stmt, DefaultEdge](loopGraph)

          // Filter i to contain only those that have some successor that is not in the loop
          val filtered = i.asScala.filter(v => Graphs.successorListOf(graph, v).asScala.exists(!vs(_)))

          // Set of nodes that jump to ends
          var ps = loopGraph.vertexSet().asScala.filter(_.nextSemantic.exists(ends(_)))

          // Take the last node.  This jumps to the "real" `e`
          while (ps.size > 1) {
            ps -= filtered.next
          }

          // Take the first one.  This jumps to the "real" `e`
          val p = ps.head
          /*println(f"  p = $p")
          println(f"  ends = $ends")
          println(f"  p.succ = ${Graphs.successorListOf(graph, p).asScala}")*/

          // The member of `ends` that is preceded by `p` is the "real" `e`
          val es = Graphs.successorListOf(graph, p).asScala.filter(!vs(_))
          assert(es.size == 1) // There should be only one
          val e = es.toList.head
          assert(ends(e))

          // Condition = nodes between c and s
          // (1) c = last node that is dominated by s (relative to e)
          // (2) c = first choke point after e and s (in reverse)
          // (3) c = statement before first edge to e or s (?)
          // *** Should take common ancestor
          // Note: (1) is the same as (2) (?)

          //println(f"  s = $s")
          //println(f"  e = $e")

          // Compute 1
          /*val cs1 = {
            val init = Graphs.predecessorListOf(graph, e).asScala.filter(vs)
            var cond = init.toSet
            var todo = init.toList
            while (todo.nonEmpty) {
              val v = todo.head
              todo = todo.tail
              if (v != c) {
                //println(f"  v = $v")
                val predecessor = Graphs.predecessorListOf(graph, v).asScala.filterNot(cond)
                //println(f"  pred = $predecessor")
                todo ++= predecessor
                cond ++= predecessor
              }
            }

            /*println(f"  (1) condition = ")
            for (s <- cond.toList.sortBy(_.index)) {
              println(f"    $s")
            }*/

            cond
          }*/

          // Compute 2
          val cs2 = {
            // Create a reversed topological traversal
            val reverseGraph = new EdgeReversedGraph(loopGraph)
            val i = new TopologicalOrderIterator[Stmt, DefaultEdge](reverseGraph)

            var seen = Set[Stmt]() // Stmts that we have seen jumps to
            var visited = Set[Stmt]() // Stmts that the topological traversal has visited
            val ps = loopGraph.vertexSet().asScala.filter(_.nextSemantic.contains(e)) // all states that jump to e

            //println(f"  ps = $ps")

            var v: Stmt = i.next
            visited += v
            //println(f"  v.init = $v")
            while (!ps.subsetOf(visited) // not after all jumps to e
              || !seen.subsetOf(visited)) {
              // not at choke point
              seen ++= Graphs.predecessorListOf(loopGraph, v).asScala

              v = i.next
              visited += v
              /*
              println(f"  v = $v")
              println(f"  seen = $seen")
              println(f"  visited = $visited")*/
            }

            /*if (!v.nextSemantic.contains(e)) {
              visited -= v
            }*/

            println(f"  (2) condition = ")
            for (s <- visited.toList.sortBy(_.index)) {
              println(f"    $s")
            }

            visited
          }

          // Compute 3
          val cs3 = {
            // Create a topological traversal
            val i = new TopologicalOrderIterator[Stmt, DefaultEdge](loopGraph)

            // Iterator on ps
            // Filter i to contain only those that have some successor that is not in the loop
            val filtered = i.asScala.filter(_.nextSemantic.contains(e))

            // Take the first condition. The real 'c' jumps to this
            val fc = filtered.next
            println(f"  fc = $fc")
            //println(f"  fc.pred = ${Graphs.predecessorListOf(graph, fc).asScala}")

            if (!fc.nextSemantic.contains(e)) {

            }

            // Finds 'c'
            val cc = Graphs.predecessorListOf(graph, fc).asScala.filter(vs)
            assert(cc.size == 1)
            val candid = cc.head
            val test = Graphs.successorListOf(graph, candid).asScala
            var c = fc
            if (test.size != 1) {
              c = candid
            }

            println(f"  c = $c")

            // Traverse from c to s:
            var cond = Set(c)
            var todo = List(c)
            while (todo.nonEmpty) {
              val v = todo.head
              todo = todo.tail
              if (v != s) {
                //println(f"  v = $v")
                val successor = Graphs.successorListOf(graph, v).asScala.filterNot(cond).filter(vs)
                //println(f"  succ = $successor")
                todo ++= successor
                cond ++= successor
              }
            }
            cond -= s

            println(f"  (3) condition = ")
            for (s <- cond.toList.sortBy(_.index)) {
              println(f"    $s")
            }

            cond
          }

          if (cs2 != cs3) {
            println("!!!!!\n!!!!!\n!!!!!\nCase 2 not equal Case 3")
          }
          if (cs2 == cs3) {
            println("Correct")
          }

        }
      }
      catch {
        case e => e.printStackTrace()
      }
    }
  }
}

// Conditions going to both exit and start == Last nodes in partial ordering
// Michael: "Break does not show, but Conitnue shows"
// Definition of Loop: Piece of code that can be run more than once
// Decompilers: Procyon - FernFlower (Intelij)
// Is there check point in conditions? If not, continue and break can be detected

class PseudoStmt(stmt: Stmt) extends Stmt(stmt.sootStmt, stmt.sootMethod) {
  override def toString: String = "PseudoStmt" + super.toString
}

// ./bin/jaam-tools loop4 --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/airplan_1.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/commons-cli-1.3.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/commons-codec-1.9.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/commons-fileupload-1.3.1.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/commons-io-2.2.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/commons-lang3-3.4.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/commons-logging-1.2.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/httpclient-4.5.1.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/httpcore-4.4.3.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/jline-2.8.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/log4j-1.2.17.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/mapdb-2.0-beta8.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/netty-all-4.0.34.Final.jar --classpath ../../engagements/en4/article/stac_engagement_4_release_v1.1/challenge_programs/airplan_1/challenge_program/lib/protobuf-java-3.0.0-beta-2.jar --classpath resources/rt.jar


// --app airplan_1.jar
// --rt ../../../../../../../../jaam/jaam.git/resources/rt.jar
// --lib commons-cli-1.3.jar
// --lib commons-codec-1.9.jar
// --lib commons-fileupload-1.3.1.jar
// --lib commons-io-2.2.jar
// --lib commons-lang3-3.4.jar
// --lib commons-logging-1.2.jar
// --lib httpclient-4.5.1.jar
// --lib httpcore-4.4.3.jar
// --lib jline-2.8.jar
// --lib log4j-1.2.17.jar
// --lib mapdb-2.0-beta8.jar
// --lib netty-all-4.0.34.Final.jar
// --lib protobuf-java-3.0.0-beta-2.jar
// 
// --app airplan_1.jar --rt ../../../../../../../../jaam/jaam.git/resources/rt.jar --lib commons-cli-1.3.jar --lib commons-codec-1.9.jar --lib commons-fileupload-1.3.1.jar --lib commons-io-2.2.jar --lib commons-lang3-3.4.jar --lib commons-logging-1.2.jar --lib httpclient-4.5.1.jar --lib httpcore-4.4.3.jar --lib jline-2.8.jar --lib log4j-1.2.17.jar --lib mapdb-2.0-beta8.jar --lib netty-all-4.0.34.Final.jar --lib protobuf-java-3.0.0-beta-2.jar
