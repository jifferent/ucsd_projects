import Words._
import Crack._
import scala.util.matching.Regex

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

case class CSE130Test(name: String, points: Int, answer: Any, thnk: () => Any)

object Test { 

  val tests: List[CSE130Test] = List(
    CSE130Test ("words",
                5,
                List ("aditya", "adivasi", "adiz", "1080"),
                () => Words ("words") take (4) toList),
    CSE130Test ("groupFreq",
                10,
                Map ("Odd" -> 5, "Even" -> 2).toList.sorted,
                () => Words.groupFreq (Words.inti.iterator, Words.isEven).toList.sorted),
    CSE130Test ("sizeFreq",
                5,
                List (13209, 1, 37559),
                () => {val wf = Words.sizeFreq ("words"); List (wf (4), wf (30), wf (12))}),
    CSE130Test ("charFreq",
                10,
                List (155630, 18107, 484449),
                () => {val cf = Words.charFreq ("words"); List (cf ('d'), cf ('z'), cf ('e'))}),
    CSE130Test ("wordsOfSize",
                5,
                List ("cyclotrimethylenetrinitramine", "trinitrophenylmethylnitramine"),
                () => Words.wordsOfSize ("words", 29) toList),
    CSE130Test ("wordsWithAllVowels",
                5,
                List ("abdomino-uterotomy", "abevacuation", "abietineous",
                      "abiogenous", "aboideau", "aboideaus"),
                () => Words.wordsWithAllVowels ("words") take (6) toList),
    CSE130Test ("wordsWithNoVowels",
                5,
                List ("1080", "10th", "1st", "2", "2,4,5-t", "2,4-d", "2d",
                      "2nd", "30-30", "3-d", "3-d", "3d", "3m", "3rd", "4-d", "4gl",
                      "4h", "4th", "5-t", "5th", "6th", "7th", "8th", "9th", "b"),
                () => Words.wordsWithNoVowels ("words") take (25) toList),
    CSE130Test ("words matching regexp 1",
                3,
                List ("a-1", "aaa", "aaa", "aae", "aaf"),
                () => Words.wordsMatchingRegexp ("words", new Regex("""^[a-z].{2}$""")) take (5) toList),
    CSE130Test ("words matching regexp 2",
                4,
                List ("caballine", "cabane", "cabbage", "cabbagelike", "cabbage-tree"),
                () => Words.wordsMatchingRegexp ("words", new Regex("""^ca.*{2}e$""")) take (5) toList),
    CSE130Test ("words matching regexp 3",
                3,
                List (),
                () => Words.wordsMatchingRegexp ("words", new Regex("""^xYx.*$""")) take (5) toList),
    CSE130Test ("transformReverse",
                5,
                List ("caterpillar", "rallipretac"),
                () => Crack.transformReverse ("caterpillar").toList.sorted),
    CSE130Test ("transformCapitalize",
                10,
                List ("taco", "tacO", "taCo", "taCO", "tAco", "tAcO", "tACo", "tACO", 
                      "Taco", "TacO", "TaCo", "TaCO", "TAco", "TAcO", "TACo", "TACO").sorted,
                () => Crack.transformCapitalize ("taco").toList.sorted),
    CSE130Test ("transformDigits",
                15,
                List ("Bow", "B0w", "6ow", "60w", "8ow", "80w").sorted,
                () => Crack.transformDigits ("Bow").toList.sorted),
    CSE130Test ("entry 1",
                5,
                "IqAFDoIjL2cDs",
                () => Entry ("checani:IqAFDoIjL2cDs:1:1:Pengpu Checani:/home/checani:/bin/bash").password),
    CSE130Test ("entry 2",
                5,
                "checani",
                () => Entry ("checani:IqAFDoIjL2cDs:1:1:Pengpu Checani:/home/checani:/bin/bash").account)
  )

  def runTests(): (Int, Int) = { 
    var points = 0
    var total = 0
    for (t <- tests) {
      total  += t.points 
      try {
        val res = t.thnk()
        if (res.toString == t.answer.toString) { 
          points += t.points
        } else println (t.name + ": returned " + res + ", expected " + t.answer)
      } catch {
        case e: Exception => println (t.name + " threw an exception: " + e)
      }
    }
    (points, total)
  }

  def apply(): Unit = {
    val (points, total) = runTests()
    println("130>> Results %d out of %d".format(points, total))
    println("130>> Compiled")
  }

  def main(args: Array[String]) = 
    apply()

}
// vim: set ts=2 sw=2 et:

