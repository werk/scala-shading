package dk.mzw.accelemation


object BindTermTest {

    var i = 0
    case class Term[T](source : String) {
        def +(other: Term[T]): Term[T] = {
            Term[T](s"($source + ${other.source})")
        }

        def |[U](other: Term[U]): Term[U] = {
            Term[U](s"($source | ${other.source})")
        }

        def flatMap[U](f: Term[T] => Term[U]) : Term[U] = {
            val v = s"v$i"
            i += 1
            Term[U](s"let $v = $source\n${f(Term[T](v)).source}")
        }

        def map[U](f: Term[T] => Term[U]) : Term[U] = flatMap(f)
    }

    implicit def fromInt(n : Int) : Term[Int] = Term[Int](s"$n")
    implicit def fromBool(b : Boolean) : Term[Boolean] = Term[Boolean](s"$b")

    def main(args: Array[String]) {
        val i1 : Term[Int] = 1

        val b1 : Term[Boolean] = true

        val t = for{
            v1 <- i1
            v2 <- v1 + 2
            v3 <- b1 + false
        } yield {
            v1 + v2 | v3
        }
        println(t.source)
    }

}
