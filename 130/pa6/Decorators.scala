object Decorators {

  object profile {

    private var cm: Map[String, Int] =  Map().withDefault(_ => 0)

    def count(name: String) =
      cm(name)

    def reset(name: String) =
      cm += name -> 0

    def apply[A, B](name: String)(f: A => B) = new Function1[A, B] {
      def apply(x: A) = {
        val n = cm(name)
        cm += name -> (1 + n)
        f(x)
      }
    }
  }

  object trace {

    private var n: Int = 0

    def apply[A, B](name: String)(f: A => B) : Function1[A, B] = new Function1[A, B] {
      def apply (x: A): B = {
        for (i <- 0 until n)
          print("| ")
        println(",- " + name + "(" + x + ")")
        n += 1
        try {
          val r = f(x)
          for (i <- 0 until (n - 1))
            print("| ")
          println("`- " + r)
          r
        } catch {
          case e: Exception =>
            throw e
        } finally {
          n -= 1
        }
      }
    }
  }

  object memo {

    def apply[A, B](f: A => B) : Function1[A, B] = new Function1[A, B] {

      private var cm: Map[A, Either[Throwable, B]] =  Map()

      def apply (x: A): B = {
        if (cm.contains(x)) {
          cm(x) match {
            case Left(e)   => throw e
            case Right(n)  => n
          }
        }
        else {
          try { 
            val r = f(x)
            cm += x -> Right(r)
            r
          } catch {
            case e: Exception =>
              cm += x -> Left(e)
              throw e
          }
        }
      }
    }
  }
}

// object memo {
// 
//   def apply[A, B](f: A => B) : Function1[A, B] = new Function1[A, B] {
// 
//     private var cm: Map[A, B] =  Map()
// 
//     def apply (x: A): B = {
//       if (cm.contains(x)) {
//         cm(x)
//       }
//       else {
//         val r = f(x)
//         cm += x -> r
//         r
//       }
//     }
//   }
// }