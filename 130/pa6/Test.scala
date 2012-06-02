import Decorators._
import org.scalacheck.Prop
import org.scalacheck.{Test => SCTest}

import java.io.PrintWriter
import java.io.File
import java.io.PrintStream


/****************************************************************/
/********************** Testing the Decorators ******************/
/****************************************************************/

object DecoratorTest {

  val fac : Int => Int = 
    profile("fac") {
      (n: Int) => if (n <= 1) { 1 } else { n * fac(n-1) }
    }

  val fib : Int => Int = 
    { case 0 => 1
      case 1 => 1
      case n => fib(n-1) + fib(n-2)
    }
  
  val fibP : Int => Int = 
    profile ("fibP") { 
      case 0 => 1
      case 1 => 1
      case n => fibP(n-1) + fibP(n-2)
    }
  
  val fibM : Int => Int = 
    memo { 
      case 0 => 1
      case 1 => 1
      case n => fibM(n-1) + fibM(n-2)
    }
  
  val fibT : Int => Int = 
    trace("fibT") { 
      case 0 => 1
      case 1 => 1
      case n => fibT(n-1) + fibT(n-2)
    }

  val fibTM : Int => Int = 
    memo { 
    trace("fibTM") { 
        case 0 => 1
        case 1 => 1
        case n => fibTM(n-1) + fibTM(n-2)
    }
    }
 
  val fibMT : Int => Int = 
    trace("fibMT") { 
    memo { 
        case 0 => 1
        case 1 => 1
        case n => fibMT(n-1) + fibMT(n-2)
    }
    }

  val fibMP : Int => Int = 
    profile ("fibMP") { 
    memo { 
        case 0 => 1
        case 1 => 1
        case n => fibMP(n-1) + fibMP(n-2)
    }
    }

  /********************** Mutually Recursive Tracing ******************/

  val even : Int => Boolean = 
    trace("even") { 
      (x: Int) => (x == 0) || (x != 1 && odd(x-1))
    }

  val odd: Int => Boolean = 
    trace("odd") { 
      (x: Int) => (x != 0) && even(x-1)
    }

  val quickSortT: List[Int] => List[Int] = 
    trace("quickSortT") {
      case Nil   => 
        Nil
      case x::Nil =>
        List(x)
      case x::xs => { 
        val left  = xs.filter(_ <= x)
        val right = xs.filter(x <  _)
        quickSortT(left) ++ List(x) ++ quickSortT(right)
      }
    }

  val quickSortMT: List[Int] => List[Int] = 
    trace("quickSortMT") {
    memo {
      case Nil   => 
        Nil
      case x::Nil =>
        List(x)
      case x::xs => { 
        val left  = xs.filter(_ <= x)
        val right = xs.filter(x <  _)
        quickSortMT(left) ++ List(x) ++ quickSortMT(right)
      }
    }
    }


  val snooze: Int => Int = 
    memo { 
      (x: Int) => { 
        val _ = Thread.sleep(5000) 
        x + 1
      }
    }
  
  class HissyFitException extends Exception
  
  val hissy : Int => Boolean = 
    memo { 
      (x: Int) => { 
        val _ = Thread.sleep(5000) 
        if (x % 2 == 0) { true } else { throw new HissyFitException } 
      }
    }




  /************************** exceptions ******************************/


  // Hint: http://www.scala-lang.org/api/current/scala/Either.html

  class OutOfChangeException extends Exception

  val changeT : ((List[Int], Int)) => List[Int] = 
    trace("changeT") { 
      case (_, 0) => 
        List()
      case (Nil, _) => 
        throw new OutOfChangeException
      case (coins, value) => {
        if (coins(0) > value) {
          changeT(coins.tail, value)
        } else {
          try { (coins(0) :: changeT(coins, value - coins(0)))} 
          catch { case _: OutOfChangeException => changeT(coins.tail, value) } 
        }
      }
    }
 
  val changeM : ((List[Int], Int)) => List[Int] = 
    trace("changeM") { 
      case (_, 0) => 
        List()
      case (Nil, _) => 
        throw new OutOfChangeException
      case (coins, value) => {
        if (coins(0) > value) {
          changeM(coins.tail, value)
        } else {
          try { (coins(0) :: changeM(coins, value - coins(0)))} 
          catch { case _: OutOfChangeException => changeM(coins.tail, value) } 
        }
      }
    }

  val changeMT : ((List[Int], Int)) => List[Int] = 
    trace("changeMT") { 
    memo {
      case (_, 0) => 
        List()
      case (Nil, _) => 
        throw new OutOfChangeException
      case (coins, value) => {
        if (coins(0) > value) {
          changeMT(coins.tail, value)
        } else {
          try { (coins(0) :: changeMT(coins, value - coins(0)))} 
          catch { case _: OutOfChangeException => changeMT(coins.tail, value) } 
        }
      }
    }
    }

  /*************************************************************************/
  /*************************************************************************/
  /*************************************************************************/

  case class DecoTest(name: String, thnk: () => Any) {
    def run() = {
      println("RUNNING %s:" format name)
      val ret = thnk()
      println("RETURNED %s" format ret.toString)
    }
  }
    
  private val qsList = List(5,8,100,45,3,89,22,78,121,2,78)

  val traceTest1 = List(
      DecoTest ("fibT(7)",      () => fibT(7)) 
    , DecoTest ("even(6)",      () => even(6))
    , DecoTest ("quicksortT",   () => quickSortT(qsList))
    )

  val memoTest1  = List(
      DecoTest ("fibMT(7)",     () => fibMT(7))
    , DecoTest ("fibTM(7)",     () => fibTM(7))
    , DecoTest ("quicksortMT1", () => quickSortMT(qsList))
    , DecoTest ("quicksortMT2", () => quickSortMT(qsList))
    )

  val memoTest2  = List(
      DecoTest ("fibMP(7)",     () => fibMP(7))
    , DecoTest ("fibMP.count",  () => profile.count("fibMP"))
    , DecoTest ("fibMP(7)",     () => fibMP(7))
    , DecoTest ("fibMP.count",  () => profile.count("fibMP")) 
    , DecoTest ("fibMP.reset",  () => profile.reset("fibMP"))
    , DecoTest ("fibMP(7)",     () => fibMP(7))
    , DecoTest ("fibMP.count",  () => profile.count("fibMP")) 
    )
 
  val traceTest2 = List(
      DecoTest ("changeT",      () => changeT(List(9,7,5), 44))
    )

  val memoTest3  = List(
      DecoTest ("changeMT1",    () => changeMT(List(9,7,5), 44))
    , DecoTest ("changeMT2",    () => changeMT(List(9,7,5), 44))
    )
 
  val allTest    = List( 
      DecoTest ("fibT(7)",      () => fibT(7)) 
    , DecoTest ("fibMT(7)",     () => fibMT(7))
    , DecoTest ("fibTM(7)",     () => fibTM(7))
    , DecoTest ("fibMP(7)",     () => fibMP(7))
    , DecoTest ("fibMP.count",  () => profile.count("fibMP"))
    , DecoTest ("fibMP(7)",     () => fibMP(7))
    , DecoTest ("fibMP.count",  () => profile.count("fibMP")) 
    , DecoTest ("fibMP.reset",  () => profile.reset("fibMP"))
    , DecoTest ("fibMP(7)",     () => fibMP(7))
    , DecoTest ("fibMP.count",  () => profile.count("fibMP")) 
    , DecoTest ("even(6)",      () => even(6))
    , DecoTest ("quicksortT",   () => quickSortT(qsList))
    , DecoTest ("quicksortMT1", () => quickSortMT(qsList))
    , DecoTest ("quicksortMT2", () => quickSortMT(qsList))
    , DecoTest ("changeT",      () => changeT(List(9,7,5), 44))
    , DecoTest ("changeMT1",    () => changeMT(List(9,7,5), 44))
    , DecoTest ("changeMT2",    () => changeMT(List(9,7,5), 44))
    )
 
  def run(tests: List[DecoTest]) = tests.foreach(_.run)
      
  def main(args: Array[String]) = run(allTest)
   
}

/************************************************************************/
/************************************************************************/
/************************************************************************/

case class CSE130Test( name: String
                     , points: Int
                     , answer: Any
                     , thnk: () => Any) 
{
  private def boolStr(b: Boolean) = if (b) "PASSED" else "FAILED"

  def printResult(b: Boolean) { 
    println("Test %s (%d points) = %s" format (name, points, boolStr(b)))
  }

  def apply(): Boolean = { 
    try { (thnk().toString == answer.toString) }
    catch { case _ => false }
  }

}

object Test { 

  def withOutFile(fn: String)(body: => Any) {
    val stream = new PrintStream(new File(fn))
    Console.withOut(stream) { body } 
  }
 
  def fileLines(f: String): List[String] = 
    scala.io.Source.fromFile(new File(f)).getLines().toList

  def eqFiles(f1: String, f2: String) : Boolean =
    try { fileLines(f1) == fileLines(f2) }
    catch { case _ => false }

  def decoTest(name: String, points: Int, resFile: String, ts: List[DecoratorTest.DecoTest]) = 
    CSE130Test(name, points, true, () => { 
      val outFile = name + ".out.tmp" 
      withOutFile(outFile) { DecoratorTest.run(ts) }
      eqFiles(resFile, outFile)
    })

  def propTest(name: String, points: Int, p: Prop) = 
    CSE130Test(name, points, true, () => SCTest.check(SCTest.Params(), p).passed)

  val tests: List[CSE130Test] = List(
    // Bst
      propTest("bso",           1, BstProperties.prop_bso)
    , propTest("contains_elt",  2, BstProperties.prop_contains_elt)
    , propTest("contains_elts", 2, BstProperties.prop_contains_elts)
    , propTest("add_elt",       3, BstProperties.prop_add_elt)
    , propTest("add_elts_old",  3, BstProperties.prop_add_elts_old)
    , propTest("multiset",      4, BstProperties.prop_multiset)
    , propTest("remove_min",    5, BstProperties.prop_remove_min)
    , propTest("remove_elt",    5, BstProperties.prop_remove_elt)
    , propTest("remove_elt_o",  5, BstProperties.prop_remove_elt_old)
    , propTest("bst_equiv_gld",10, BstProperties.prop_bst_equiv_gold)

    // Decorators 
    , decoTest("traceTest1",   14, "traceTest1.out", DecoratorTest.traceTest1)
    , decoTest("traceTest2",    6, "traceTest2.out", DecoratorTest.traceTest2)
    , decoTest("memoTest1",     5, "memoTest1.out",  DecoratorTest.memoTest1)
    , decoTest("memoTest2",     5, "memoTest2.out",  DecoratorTest.memoTest2)
    , decoTest("memoTest3",    10, "memoTest3.out",  DecoratorTest.memoTest3)
 
    // Doc
    , propTest("hcatT_width",   2, DocProperties.prop_hcatT_width)
    , propTest("hcatT_height",  2, DocProperties.prop_hcatT_height)
    , propTest("hcatB_width",   2, DocProperties.prop_hcatB_width)
    , propTest("hcatB_height",  2, DocProperties.prop_hcatB_height)
    , propTest("vcat_width",    2, DocProperties.prop_vcat_width)
    , propTest("vcat_height",   2, DocProperties.prop_vcat_height)

    // Render (TBD)

    // Json
    , CSE130Test("jv0", 2, true, () => JsonTest.jvals0 == JsonTest.jvReals(0))
    , CSE130Test("jv1", 2, true, () => JsonTest.jvals1 == JsonTest.jvReals(1))
    , CSE130Test("jv2", 2, true, () => JsonTest.jvals2 == JsonTest.jvReals(2))
    , CSE130Test("jv3", 2, true, () => JsonTest.jvals3 == JsonTest.jvReals(3))
    , CSE130Test("jv4", 2, true, () => JsonTest.jvals4 == JsonTest.jvReals(4))
    , CSE130Test("jv5", 2, true, () => JsonTest.jvals5 == JsonTest.jvReals(5))
    , CSE130Test("jv6", 2, true, () => JsonTest.jvals6 == JsonTest.jvReals(6))
    , CSE130Test("jv7", 2, true, () => JsonTest.jvals7 == JsonTest.jvReals(7))
    , CSE130Test("jv8", 2, true, () => JsonTest.jvals8 == JsonTest.jvReals(8))
  )

  def runTests(tests: List[CSE130Test]): (Int, Int) = { 
    var points = 0
    var total = 0
    for (t <- tests) {
      total  += t.points 
      val ok  = t() 
      t.printResult(ok)
      if (ok) { points += t.points }
    }
    (points, total)
  }

  def apply(): Unit = {
    val (points, total) = runTests(tests)
    println("130>> Results %d out of %d".format(points, total))
    println("130>> Compiled")
  }

  def main(args: Array[String]) = apply()

}

// vim: set ts=2 sw=2 et:

