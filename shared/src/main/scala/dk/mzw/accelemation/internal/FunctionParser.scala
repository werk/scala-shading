package dk.mzw.accelemation.internal

// TODO write a better/correct parser
object FunctionParser {

    sealed trait DecomposedDeclaration
    case class DecomposedFunction(
        name : String,
        returnType : String,
        typedArguments : List[(String, String)],
        body : List[String]
    ) extends DecomposedDeclaration

    case class DecomposedConstant(
        name : String,
        returnType : String,
        value : String
    ) extends DecomposedDeclaration

    def parse(functions : String) : List[DecomposedDeclaration] = {
        val lines = functions.split("""\s*[\n\r]+\s*""")
        val noComments = lines.filterNot(_.startsWith("//")).mkString("\n")
        val in = new ConsumableString(noComments)
        var decomposed = List[DecomposedDeclaration]()
        while(in.s.nonEmpty) {
            decomposed ::= parseFunction(in)
        }
        decomposed.reverse
    }

    private def parseFunction(in : ConsumableString) : DecomposedDeclaration = {
        in.dropWhile(_.isWhitespace)
        val returnType = in.takeWhile(_.isLetterOrDigit).mkString
        if(returnType.toLowerCase == "const") {
            in.dropWhile(_.isWhitespace)
            val theType = in.takeWhile(_.isLetterOrDigit).mkString
            in.dropWhile(_.isWhitespace)
            val name = in.takeWhile(_.isLetterOrDigit).mkString
            in.dropWhile(c => c.isWhitespace || c == '=')
            val value = in.takeWhile(_ != ';').mkString
            in.dropWhile(c => c.isWhitespace || c == ';')
            DecomposedConstant(name, theType, value)
        } else {
            in.dropWhile(_.isWhitespace)
            val name = in.takeWhile(_.isLetterOrDigit).mkString
            in.dropWhile(c => c.isWhitespace || c == '(')
            val parameters = in.takeWhile(_ != ')').mkString
            val arguments = parameters.trim.split("""\s*,\s*""").map(_.split("""\s+""") match {
                case Array(t, v) => (t, v)
                case a => throw new RuntimeException(s"Failed to parse parameters '$parameters'")
            }).toList
            in.dropWhile(c => c == ')' || c.isWhitespace || c == '{')
            val body = in.takeWhile(_ != '}').mkString
            val bodyLines = body.trim.split("""\s*[\n\r]+\s*""").toList
            in.dropWhile(c => c.isWhitespace || c == '}')
            DecomposedFunction(name, returnType, arguments, bodyLines)
        }
    }

    def reassemble(d : DecomposedDeclaration) : String = d match {
        case d : DecomposedFunction => reassemble(d)
        case d : DecomposedConstant => reassemble(d)
    }

    def reassemble(c : DecomposedConstant) : String = {
        s"""const ${c.returnType} ${c.name} = ${c.value};\n"""
    }

    def reassemble(f : DecomposedFunction) : String = {
        s"""${f.returnType} ${f.name}(${f.typedArguments.map{case (t, v) => s"$t $v"}.mkString(", ")}) {
${f.body.map("    " + _).mkString("\n")}
}
"""
    }

    private class ConsumableString(var s : String) {
        def takeWhile(p : Char => Boolean) : String = {
            val part = s.takeWhile(p)
            s = s.drop(part.length)
            part
        }
        def dropWhile(p : Char => Boolean) : Unit = takeWhile(p)
    }
}

