import scala.collection.immutable.TreeSet

/*******************************************************************************/
/******************** Specification ********************************************/
/*******************************************************************************/

trait AbstractSet[A] extends Iterable[A] {
  def contains(n: A): Boolean
  def add(n: A): AbstractSet[A]
  def remove(n: A): AbstractSet[A]

  // For Testing
  def execCmds(cmds: List[(Boolean, A)]): AbstractSet[A] = {
    cmds.foldLeft(this)((s, cmd) => {
      if (cmd._1) { this.add(cmd._2) } else { this.remove(cmd._2) }
    })
  }
}

/*******************************************************************************/
/******************** Reference Implementation *********************************/
/*******************************************************************************/

case class GoldSet[A <% Ordered[A]](elts: TreeSet[A])
  extends AbstractSet[A]
{
  def contains(n: A) = this.elts.contains(n)
  def add(n: A)      = GoldSet(this.elts + n)
  def remove(n: A)   = GoldSet(this.elts - n)
  def iterator       = this.elts.iterator
}

/*******************************************************************************/
/******************** BST-Based Implementation *********************************/
/*******************************************************************************/

case class Node[A <% Ordered[A]](elt: A, left: BST[A], right: BST[A]) extends BST[A]
case class Leaf[A <% Ordered[A]]() extends BST[A]
sealed abstract class BST[A <% Ordered[A]] extends AbstractSet[A]
{
  override def forall(f: A => Boolean): Boolean =
    this match {
      case Leaf ()        => true
      case Node (e, l, r) => f(e) && l.forall(f) && r.forall(f)
    }

  def isOrdered : Boolean =
    this match {
      case Leaf()        => true
      case Node(e, l, r) => l.forall(_ < e) && r.forall(e < _)
    }

  def contains(x: A): Boolean =
    this match {
      case Leaf()         => false
      case Node(e, l, r)  => if (x == e) true else l.contains(x) || r.contains(x)
    }

  def fold[B](f: (B, A) => B, acc: B): B =
    this match {
      case Leaf()         => acc
      case Node(e, l, r)  => r.fold(f, f(l.fold(f, acc), e))
    }

  def iterator : Iterator[A] = {
    this
    .fold(((elts : List[A], x) =>  x::elts), List())
    .reverse
    .iterator
  }

  def getElts: List[A] =
    this match {
      case Leaf () => List()
      case Node (e, l, r) => l.getElts ++  List(e) ++ r.getElts
    }

  override def toString : String = {
    "BST(" + getElts.mkString(", ")  + ")"
  }

  def add(x:A): BST[A] =
    this match {
      case Leaf()         => Node(x, Leaf(), Leaf())
      case Node(e, l, r)  =>
        if (e == x)
          Node(x, l, r)
        else if (x < e)
          Node(e, l.add(x), r)
        else
          Node(e, l, r.add(x))
    }

  def removeMin : (A, BST[A]) = {
    this match {
      case Leaf()             => sys.error("Empty Tree")
      case Node(e, Leaf(), r) => (e, r)
      case Node(e, l, r)      => val (elt, t) = l.removeMin
        (elt, Node(e, t, r))
    }
  }

  def remove(x: A): BST[A] = {
    this match {
      case Leaf()         => Leaf()
      case Node(_ ,_ ,_)  =>
        val (e, t) = this.removeMin
        if (x == e)
          t
        else
          t.remove(x).add(e)
    }
  }
}

object BST {

  def t0 : BST[Int] = Leaf ()
  def t1 : BST[Int] = Node (20, Node (10, t0, t0), Node (30, t0, t0))
  def t2 : BST[Int] = Node (5, t0, t1)

  def apply[A <% Ordered[A]](xs: Iterable[A]) : BST[A] = {
    val s0 : BST[A] = Leaf ()
    xs.iterator.foldLeft(s0)((s, x) => s.add(x))
  }

  def build[A <% Ordered[A]](xs: List[A]): BST[A] = {
    if (xs.length == 0) {
      Leaf ()
    } else if (xs.length == 1) {
      Node(xs(0), Leaf (), Leaf())
    } else {
      xs.distinct match {
        case x :: xs  =>
          val (left, right) = xs partition (_ <= x)
          Node(x, build(left), build(right))
        case _        => Leaf ()
      }
    }
  }
}

/*************************************************************************/
/***************** Property Based Testing ********************************/
/*************************************************************************/

import org.scalacheck._
import org.scalacheck.Prop._
import Gen._
import Arbitrary.arbitrary

object BstProperties extends Properties("BST") {

  def genInt(lo: Int, hi: Int): Gen[Int] =
    Gen.choose(lo, hi)

  def genInts(lo: Int, hi: Int): Gen[List[Int]] =
    Gen.containerOf[List, Int](Gen.choose(lo, hi))

  def genIntBst(lo: Int, hi: Int): Gen[BST[Int]] =
    for { xs <- genInts(lo, hi) } yield BST.build(xs)

  implicit def arbIntBst: Arbitrary[BST[Int]] =
    Arbitrary { genIntBst(0, 100) }

  def arbIntCmd: Gen[List[(Boolean, Int)]] = {
    val genIntCmd = for { b <- arbitrary[Boolean]
                        ; i <- genInt(0, 15)} //Gen.choose(0, 15)
                    yield (b, i)
    Gen.containerOf[List, (Boolean, Int)](genIntCmd)
  }

  //Holds after build
  val prop_bso = forAll((t: BST[Int]) =>
    t.isOrdered
  )

  //Holds after "build" and "contains"
  val prop_contains_elt = forAll (genInt(0, 20), genInts(0, 20)) {
    (x: Int, xs: List[Int]) => xs.contains(x) == BST.build(xs).contains(x)
  }

  //Holds after "contains" and "fold"
  val prop_contains_elts = forAll((t: BST[Int]) =>
    t.toList.forall(t contains _)
  )

  //Holds after "add"
  val prop_add_elt = forAll((t: BST[Int], elt: Int) =>
    t.add(elt).contains(elt)
  )

  //Holds after "add"
  val prop_add_elts_old = forAll((t: BST[Int], elt: Int) =>
    t.forall(t.add(elt).contains(_))
  )

  //Holds after "add": Fix this property
  val prop_multiset = forAll((xs: List[Int]) =>
    BST(xs).toList == xs.distinct.sorted
  )

  //Holds after "removeMin"
  val prop_remove_min = forAll((t: BST[Int]) => {
      ! t.isEmpty ==> { val (x, t_new) = t.removeMin
                        t.contains(x) && t_new.forall(x < _) }
  })

  //Holds after "remove"
  val prop_remove_elt = forAll((t: BST[Int], elt: Int) =>
    ! (t.remove(elt).contains(elt))
  )

  //Holds after "remove"
  val prop_remove_elt_old = forAll((t: BST[Int], elt: Int) =>
    t.forall((x => x == elt || t.remove(elt).contains(x)))
  )

  //Holds after "add" and "remove"
  val prop_bst_equiv_gold = forAll (arbIntCmd) { cmds =>
    val g0 = GoldSet(TreeSet[Int]())
    val b0 = BST(List[Int]())
    g0.execCmds(cmds).toList == b0.execCmds(cmds).toList
  }

  // run all properties with:
  // $ scala BstProperties
  property("bso")            = prop_bso
  property("contains_elt")   = prop_contains_elt
  property("contains_elts")  = prop_contains_elts
  property("add_elt")        = prop_add_elt
  property("add_elts_old")   = prop_add_elts_old
  property("multiset")       = prop_multiset
  property("remove_min")     = prop_remove_min
  property("remove_elt")     = prop_remove_elt
  property("remove_elt_old") = prop_remove_elt_old
  property("bst_equiv_gold") = prop_bst_equiv_gold

}
