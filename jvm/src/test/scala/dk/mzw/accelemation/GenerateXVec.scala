package dk.mzw.accelemation

object GenerateXVec {

    def main(args: Array[String]) {

        generate(2, "xyzw", "rgba", "stpq")
        generate(3, "xyzw", "rgba", "stpq")
        generate(4, "xyzw", "rgba", "stpq")

        def generate(n : Int, axesList : String*): Unit = {
            def line(a : String) = s"""        def $a : Self${a.length} = make${a.length}(Field("$a", untyped))"""
            def block(axes : String) : Unit = {
                val letters = axes.take(n)
                val newLetters = (axes(n-1).toString + (if(n == 2) axes(0) else "")).toList
                for{
                    a <- letters
                    label = s"$a"
                    if label.exists(newLetters.contains)
                } println(line(label))

                for{
                    a <- letters
                    b <- letters
                    label = s"$a$b"
                    if label.exists(newLetters.contains)
                } println(line(label))

                for{
                    a <- letters
                    b <- letters
                    c <- letters
                    label = s"$a$b$c"
                    if label.exists(newLetters.contains)
                } println(line(label))

                for{
                    a <- letters
                    b <- letters
                    c <- letters
                    d <- letters
                    label = s"$a$b$c$d"
                    if label.exists(newLetters.contains)
                } println(line(label))
            }

            println(s"    sealed trait XVec$n[Self1, Self2, Self3, Self4] extends XVec${if(n > 2) (n - 1).toString else ""}[Self1, Self2, Self3, Self4] {")
            axesList.zipWithIndex.foreach{case (a, i) =>
                block(a)
                if(i != axesList.length-1) println()
            }
            println(s"    }")
            println()
        }

    }
}
