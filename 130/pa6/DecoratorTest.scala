import DecoratorTest._
import Decorators._

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

  private val qsList = List(5,8,100,45,3,89,22,78,121,2,78)
  
  private val tests  = List( 
      ("fibT(7)",      () => fibT(7)) 
    , ("fibMT(7)",     () => fibMT(7))
    , ("fibTM(7)",     () => fibTM(7))
    , ("fibMP(7)",     () => fibMP(7))
    , ("fibMP.count",  () => profile.count("fibMP"))
    , ("fibMP(7)",     () => fibMP(7))
    , ("fibMP.count",  () => profile.count("fibMP")) 
    , ("fibMP.reset",  () => profile.reset("fibMP"))
    , ("fibMP(7)",     () => fibMP(7))
    , ("fibMP.count",  () => profile.count("fibMP")) 
    , ("even(6)",      () => even(6))
    , ("quicksortT",   () => quickSortT(qsList))
    , ("quicksortMT1", () => quickSortMT(qsList))
    , ("quicksortMT2", () => quickSortMT(qsList))
    , ("changeT",      () => changeT(List(9,7,5), 44))
    , ("changeMT1",    () => changeMT(List(9,7,5), 44))
    , ("changeMT2",    () => changeMT(List(9,7,5), 44))
  )
 
  def main(args: Array[String]) = {
    for ( (str, f) <- tests ) {
      println("RUNNING %s:" format str)
      val ret = f()
      println("RETURNED %s" format ret.toString)
    }
  }

}


// vim: set ts=2 sw=2 et:

