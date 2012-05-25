import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import Lines._

object Words {

  def apply(file: String) : Iterator[String] =
    Lines.iterator(file).map(_.toLowerCase)

  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] = {
    var freqMap = new HashMap[B, Int]
    for (x <- xs) {
      val r = f(x)
      freqMap += r -> (1 + freqMap.getOrElse(r, 0))
    }
    freqMap
  }

  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" }

  def sizeFreq(file: String): HashMap[Int, Int] =
    groupFreq(Words(file), (x : String) => x.length)

  def charFreq(file: String): HashMap[Char, Int] =
  {
    val chars   = Words(file).flatMap(_.toIterator)
    val grouper = (x : Char) => x
    groupFreq(chars, grouper)
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] =
    Words(file).filter(_.length == size)

  def wordsWithAllVowels(file: String): Iterator[String] =
    Words(file).filter((x : String) => (x.contains("a") && x.contains("e") && x.contains("i") && x.contains("o") && x.contains("u")))

  def wordsWithNoVowels(file: String): Iterator[String] =
    Words(file).filter((x : String) => (!x.contains("a") && !x.contains("e") && !x.contains("i") && !x.contains("o") && !x.contains("u")))

  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] =
    Words(file).flatMap((x : String) => (re findAllIn x))

}

// vim: set ts=2 sw=2 et:

