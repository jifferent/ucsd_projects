import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io.PrintWriter

case class Entry ( account   : String
                 , password  : String
                 , uid       : Int
                 , gid       : Int
                 , gecos     : String
                 , directory : String
                 , shell     : String
                 )

object Entry {

  def apply(line: String) : Entry = {
    val t = line.split(':')
    new Entry(t(0), t(1), t(2).toInt, t(3).toInt, t(4), t(5), t(6))
  }

}

object Crack {

  def transformReverse(w: String) : Iterator[String] = {
    List(w, w.reverse).iterator
  }

  def transformCapitalize(w: String) : Iterator[String] = {
    var s = new StringBuilder(w.toLowerCase)
    var xs = scala.collection.mutable.MutableList(s)

    for (i <- 0 to (s.length - 1)) {
      var temp = new scala.collection.mutable.MutableList[StringBuilder]
      for (x <- xs) {
        var new_s = new StringBuilder(x.toString)
        temp += new_s.replace(i, i + 1, new_s(i).toUpper.toString)
      }
      xs = xs ++ temp
    }

    xs.map(_.toString).iterator
  }

  def transformDigits(w:String) : Iterator[String] = {
    var s = new StringBuilder(w)
    var xs = scala.collection.mutable.MutableList(s)

    for (i <- 0 to (w.length - 1)) {
      var temp = new scala.collection.mutable.MutableList[StringBuilder]
      for (x <- xs) {
        var new_s = new StringBuilder(x.toString)
        new_s(i).toLower match {
          case 'o'  => temp += new_s.replace(i, i + 1, "0")
          case 'i'  => temp += new_s.replace(i, i + 1, "1")
          case 'l'  => temp += new_s.replace(i, i + 1, "1")
          case 'z'  => temp += new_s.replace(i, i + 1, "2")
          case 'e'  => temp += new_s.replace(i, i + 1, "3")
          case 'a'  => temp += new_s.replace(i, i + 1, "4")
          case 's'  => temp += new_s.replace(i, i + 1, "5")
          case 't'  => temp += new_s.replace(i, i + 1, "7")
          case 'g'  => temp += new_s.replace(i, i + 1, "9")
          case 'q'  => temp += new_s.replace(i, i + 1, "9")
          case 'b'  => {
            temp += new_s.replace(i, i + 1, "6")
            temp += new StringBuilder(x.toString).replace(i, i + 1, "8")
          }
          case _    => new_s(i)
        }
      }
      xs = xs ++ temp
    }
    xs.map(_.toString).iterator
  }

  def checkPassword(plain: String, enc: String) : Boolean =
    Crypt.crypt(enc, plain) == enc

  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  def forCandidates(w: String, e: Entry, fd: PrintWriter, candidates: List[String]): Boolean = {
    for (c <- candidates) {
      if (checkPassword(c, e.password)) {
        fd.println("%s=%s".format(e.account, c))
        fd.flush
        return true
      }
    }
    false
  }

  // scala> Crack("passwd", "words", "soln.1")
  // goto> scala Crack passwd words soln.1
  def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {
    var words         = candidateWords(wordsFile)
    val fd            = new PrintWriter(outFile)
    var guessed_pwds  = List("")

    var pwds = Lines.iterator(pwdFile).map((x : String) => Entry(x))
    for (w <- words) {
      val candidates = transformReverse(w).toList
      pwds = Lines.iterator(pwdFile).map((x : String) => Entry(x))
      for (e <- pwds) {
        if (forCandidates(w, e, fd, candidates)) {
          guessed_pwds = e.password :: guessed_pwds
        }
      }
    }

    words = candidateWords(wordsFile)
    for (w <- words) {
      pwds = Lines.iterator(pwdFile).map((x : String) => Entry(x))
      for (e <- pwds) {
        if (!guessed_pwds.contains(e.password)) {
          val candidates = (transformCapitalize(w) ++ transformDigits(w)).toList
          forCandidates(w, e, fd, candidates)
        }
      }
    }

    fd.close
  }

  def main(args: Array[String]) = {
    println("Begin: Cracking Passwords")
    apply(args(0), args(1), args(2))
    println("Done: Cracking Passwords")
  }
}

// vim: set ts=2 sw=2 et:

