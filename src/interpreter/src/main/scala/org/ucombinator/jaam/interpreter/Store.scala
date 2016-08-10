package org.ucombinator.jaam.interpreter

import scala.collection.mutable
import scala.reflect.ClassTag

trait AbstractDomain[T] {
  val bot : T
  def joinLeft(d1 : T, d2 : T) : Option[T]
  def join(d1 : T, d2 : T) : T
}
/*
trait AbstractDomain[T <: AbstractDomain[T]] {
  val bot : T
  def maybeJoin(d: T): Option[T]
}
 */

/* mutable generic store */
abstract class AbstractStore[K <: Addr, V](val map: mutable.Map[K, V], abstractDomain : AbstractDomain[V]) {
  var on: Boolean = false
  var readAddrs = Set[K]()
  var writeAddrs = Set[K]()

  def resetReadAddrsAndWriteAddrs(): Unit = {
    readAddrs = Set[K]()
    writeAddrs = Set[K]()
  }

  /////////////////////////////////

  def contains(addr: K): Boolean = map.contains(addr)

  def join(store: AbstractStore[K, V]): AbstractStore[K, V] = {
    store.map.foreach { case (k, v) => {
      this.update(k, v)
    }}
    this
  }

  def get(addr: K): Option[V] = {
    readAddrs += addr
    map.get(addr)
  }
  def getOrElseBot(addr: K): V = {
    this.get(addr) match {
      case Some(value) => value
      case None => abstractDomain.bot
    }
  }

  def update(m: mutable.Map[K, V]): AbstractStore[K, V] = {
    m.foreach{ case (a, d) => this.update(a, d) }
    this
  }
  def update(addrs: Set[K], d: V): AbstractStore[K, V] = {
    addrs.foreach{ case a => this.update(a, d) }
    this
  }
  def update(addrs: Option[Set[K]], d: V): AbstractStore[K, V] = {
    addrs match {
      case Some(as) => this.update(as, d)
      case None =>
    }
    this
  }
  def update(addr: K, d: V): AbstractStore[K, V] = {
    val oldd: V = this.getOrElseBot(addr)

    abstractDomain.joinLeft(oldd, d) match {
      case None => {}
      case Some(n) => if (on) { writeAddrs += addr }; map += (addr -> n)
    }
    this
  }

  def apply(addr: K): V = {
    readAddrs += addr
    map(addr)
  }
  def apply(addrs: Set[K]): V = {
    readAddrs ++= addrs
    val ds = for (a <- addrs; if map.contains(a)) yield { map(a) }
    val res = ds.fold(abstractDomain.bot)(abstractDomain.join)
    if (res == abstractDomain.bot) {
      throw UndefinedAddrsException(addrs)
    }
    res
  }

  def prettyString() : List[String] =
    (for ((a, d) <- map) yield { a + " -> " + d }).toList
}

object ValueDomain extends AbstractDomain[D] {
  val bot = D(Set())
  // TODO: use a more efficient algorithm
  def joinLeft(d1 : D, d2 : D) = {
    val d3 = d1.join(d2)
    if (d3.values.size == d1.values.size) None
    else Some(d3)
  }

  def join(d1 : D, d2 : D) = d1.join(d2)
}

object KontDomain extends AbstractDomain[KontD] {
  val bot = KontD(Set())
  // TODO: use a more efficient algorithm
  def joinLeft(d1 : KontD, d2 : KontD) = {
    val d3 = d1.join(d2)
    if (d3.values.size == d1.values.size) None
    else Some(d3)
  }

  def join(d1 : KontD, d2 : KontD) = d1.join(d2)
}

case class Store(override val map: mutable.Map[Addr, D])
  extends AbstractStore[Addr, D](map, ValueDomain)

case class KontStore(override val map: mutable.Map[KontAddr, KontD])
  extends AbstractStore[KontAddr, KontD](map, KontDomain)
