
class Doc(val lines: List[String]) {

  def width  = lines.map(_.length).padTo(1, 0).max
  def height = lines.length
  override def toString = lines.mkString("\n")

  def widen(ls: List[String]) =
    ls.map(l => l + " " * (width - l.length))

  def hcatT(that: Doc) : Doc = {
    new Doc(this.lines.zipAll(that.lines, "", "").map((x : (String, String)) => (x._1 + " " * (this.width - x._1.length) + x._2)))
  }

  def hcatB(that: Doc) : Doc = {
    new Doc(this.lines.reverse.zipAll(that.lines.reverse, "", "").map((x : (String, String)) => (x._1 + " " * (this.width - x._1.length) + x._2)).reverse)
  }

  def vcat(that: Doc) : Doc = {
    new Doc(this.lines ++ that.lines)
  }
}

object Doc {
  def apply(s: String)         = new Doc(List(s))
  def apply(xs: List[String])  = new Doc(xs)
  def apply(xs: String*)       = new Doc(xs.toList)
  def empty : Doc              = new Doc(List())
  def vcats(ds: List[Doc]):Doc = ds.foldLeft(empty)(_.vcat(_))

  def vcats(ds: List[Doc], start: Doc, sep: Doc, end: Doc): Doc =
    ds match {
      case Nil      => start hcatT end
      case d :: Nil => start hcatT d hcatB end
      case d :: _   => (start hcatT d) vcat (vcats(ds.tail.map(sep hcatT _)) hcatB end)
    }

  // Should be tail-recursive
  def padBegin[A](xs: List[A], n: Int, x:A): List[A] = {
    if (xs.length < n)
      padBegin(x::xs, n, x)
    else
      xs
  }

  // Should be tail-recursive (or not at all recursive!)
  def padEnd[A](xs: List[A], n: Int, x:A): List[A] = {
    xs.padTo(n, x)
  }
}

/******************************************************************/
/*************** The Json TypeClass *******************************/
/******************************************************************/

sealed abstract class JVal
case class JStr(s: String)            extends JVal
case class JNum(n: BigDecimal)        extends JVal
case class JObj(o: Map[String, JVal]) extends JVal
case class JArr(a: List[JVal])        extends JVal

object JVal {

  private def renderDocs(ds: List[Doc]) =
    Doc.vcats(ds, Doc("{ "), Doc(", "), Doc(" }"))

  def render(jv: JVal) : Doc = {
    jv match {
      case JStr(s) => Doc("\"%s\"".format(s))
      case JNum(n) => Doc(n.toString)
      case JObj(o) => renderDocs(o.map((x: (String, JVal)) => (Doc("%s : ".format(x._1)).hcatT(render(x._2)))).toList)
      case JArr(a) => renderDocs(a.map((x: JVal)           => (render(x))))
    }
  }
}

/******************************************************************/
/*************** The Json TypeClass *******************************/
/******************************************************************/

//http://marakana.com/s/scala_typeclasses,1117/index.html

trait Json {
  def json: JVal
}

object JsonWriter {
  def write(v: JVal): String =
    JVal.render(v).toString
  def write[A <% Json](x: A): String =
    write(x.json)
}

/******************************************************************/
/*************** Creating Json Instances **************************/
/******************************************************************/

object Json {

  implicit def stringJson(s: String) =
    new Json {
      def json: JVal = JStr(s)
    }

  implicit def intJson(i: Int) =
    new Json {
      def json: JVal = JNum(i)
    }

  implicit def tup2Json[A1 <% Json, A2 <% Json](p: (A1, A2)) =
    new Json {
      def json: JVal = JObj(Map(
         "fst" -> p._1.json
       , "snd" -> p._2.json
      ))
    }

  implicit def tup3Json[A1 <% Json, A2 <% Json, A3 <% Json](p: (A1, A2, A3)) : Json = {
    new Json {
      def json: JVal = JObj(Map(
          "fst" -> p._1.json
        , "snd" -> p._2.json
        , "thd" -> p._3.json
      ))
    }
  }

  implicit def listJson[A <% Json](xs: List[A]) : Json = {
    new Json {
      def json: JVal = JArr(xs.map(_.json))
    }
  }

  implicit def arrJson[A <% Json](xs: Array[A]) : Json = {
    new Json {
      def json: JVal = JArr(xs.toList.map(_.json))
    }
  }

  implicit def mapJson[A <% Json](m: Map[String, A]) : Json = {
    new Json {
      def json: JVal = JObj(m.map((x: (String, A)) => (x._1 -> x._2.json)))
    }
  }
}

/******************************************************************/
/********* Converting and Rendering Scala Values as Json **********/
/******************************************************************/

object JsonTest {
  import Json._
  val tup0 = (1, "cat")
  val tup1 = (tup0, tup0, tup0)
  val tup2 = (1, "cat", tup1)
  val tup  = (((1, (2, (3, (4, (5, "Nil")))))))
  val intl = List(("one", 1), ("two", 2), ("three", 3))
  val ints = Array(1, 2, 3, 4)
  val mm   = Map("mon" -> 10, "tue" -> 20, "wed" -> 30)
  val lang = Map( "eng" -> List(("one", 1), ("two", 2), ("three", 3))
                , "spanish" -> List(("uno", 1), ("dos", 2), ("tres", 3))
                , "french" -> List(("un", 1) , ("deux", 2), ("trois", 3)))

  val trip = (ints, mm, lang)

  lazy val jvals0 = tup0.json
  lazy val jvals1 = tup1.json
  lazy val jvals2 = tup2.json
  lazy val jvals3 = tup.json
  lazy val jvals4 = intl.json
  lazy val jvals5 = ints.json
  lazy val jvals6 = mm.json
  lazy val jvals7 = lang.json
  lazy val jvals8 = trip.json


  // Real JVals
  val jvReals = List(
      JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat")))

    , JObj(Map("fst" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat")))
              ,"snd" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat")))
              ,"thd" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat")))))

    , JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat"), "thd" -> JObj(Map("fst" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat"))), "snd" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat"))), "thd" -> JObj(Map("fst" -> JNum(1), "snd" -> JStr("cat")))))))

    , JObj(Map("fst" -> JNum(1), "snd" -> JObj(Map("fst" -> JNum(2), "snd" -> JObj(Map("fst" -> JNum(3), "snd" -> JObj(Map("fst" -> JNum(4), "snd" -> JObj(Map("fst" -> JNum(5), "snd" -> JStr("Nil")))))))))))

    , JArr(List(JObj(Map("fst" -> JStr("one"), "snd" -> JNum(1)))
              , JObj(Map("fst" -> JStr("two"), "snd" -> JNum(2)))
              , JObj(Map("fst" -> JStr("three"), "snd" -> JNum(3)))))

    , JArr(List(JNum(1), JNum(2), JNum(3), JNum(4)))

    , JObj(Map("mon" -> JNum(10), "tue" -> JNum(20), "wed" -> JNum(30)))

    , JObj(Map("eng" -> JArr(List(JObj(Map("fst" -> JStr("one"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("two"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("three"), "snd" -> JNum(3))))), "spanish" -> JArr(List(JObj(Map("fst" -> JStr("uno"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("dos"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("tres"), "snd" -> JNum(3))))), "french" -> JArr(List(JObj(Map("fst" -> JStr("un"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("deux"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("trois"), "snd" -> JNum(3)))))))


    , JObj(Map("fst" -> JArr(List(JNum(1), JNum(2), JNum(3), JNum(4))), "snd" -> JObj(Map("mon" -> JNum(10), "tue" -> JNum(20), "wed" -> JNum(30))), "thd" -> JObj(Map("eng" -> JArr(List(JObj(Map("fst" -> JStr("one"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("two"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("three"), "snd" -> JNum(3))))), "spanish" -> JArr(List(JObj(Map("fst" -> JStr("uno"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("dos"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("tres"), "snd" -> JNum(3))))), "french" -> JArr(List(JObj(Map("fst" -> JStr("un"), "snd" -> JNum(1))), JObj(Map("fst" -> JStr("deux"), "snd" -> JNum(2))), JObj(Map("fst" -> JStr("trois"), "snd" -> JNum(3)))))))))
  )

}

/*************************************************************************/
/***************** Property Based Testing ********************************/
/*************************************************************************/

import org.scalacheck._
import org.scalacheck.Prop._
import Gen._
import Arbitrary.arbitrary

object DocProperties extends Properties("Doc") {

  def genChar: Gen[Char] =
    Gen.choose(97, 122).map(_.toChar)

  def genString: Gen[String] =
    Gen.containerOf[List, Char](genChar).map(_.mkString(""))

  implicit def arbDoc: Arbitrary[Doc] =
    Arbitrary {
      for (ls <- Gen.containerOf[List, String](genString))
      yield Doc(ls)
    }

  val prop_hcatT_width = forAll((d1: Doc, d2: Doc) =>
    (d1 hcatT d2).width == d1.width + d2.width
  )

  val prop_hcatT_height = forAll((d1: Doc, d2: Doc) =>
    (d1 hcatT d2).height == (d1.height max d2.height)
  )

  val prop_hcatB_width = forAll((d1: Doc, d2: Doc) =>
    (d1 hcatB d2).width == d1.width + d2.width
  )

  val prop_hcatB_height = forAll((d1: Doc, d2: Doc) =>
    (d1 hcatB d2).height == (d1.height max d2.height)
  )

  val prop_vcat_width = forAll((d1: Doc, d2: Doc) =>
    (d1 vcat d2).width == (d1.width max d2.width)
  )

  val prop_vcat_height = forAll((d1: Doc, d2: Doc) =>
    (d1 vcat d2).height == d1.height + d2.height
  )

  // run all properties with:
  // $ scala DocProperties
  property("prop_hcatB_height") = prop_hcatB_height
  property("prop_hcatB_width")  = prop_hcatB_width
  property("prop_hcatT_height") = prop_hcatT_height
  property("prop_hcatT_width")  = prop_hcatT_width
  property("prop_vcat_height")  = prop_vcat_height
  property("prop_vcat_width")   = prop_vcat_width


}
// vim: set ts=2 sw=2 et:
