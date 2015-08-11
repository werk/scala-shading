package dk.mzw.accelemation.js

import dk.mzw.accelemation.Internal._
import dk.mzw.accelemation.Language.{Image, Time, R, Color, Term}
import dk.mzw.accelemation.ToGlsl
import dk.mzw.accelemation.samples._
import org.scalajs.dom.raw.MouseEvent

import scala.scalajs.js.JSApp
import org.scalajs.dom

object Main extends JSApp {
    val animations = List(
        TimeLens.apply,
        HidingDevils.apply,
        Spiral.apply
    )

    def main(): Unit = {
        val glsl = new GlslContainer(dom.document.body)
        var i = 0
        glsl.load(ToGlsl(animations(i)))

        dom.document.body.onmousedown = {e : MouseEvent =>
            i = (i + 1) % animations.length
            val animation = animations(i)
            val code = ToGlsl(animation)
            glsl.load(code)
            val hoas : FunTerm[Double => Double => Double => (Double, Double, Double, Double)] = arrow(animation)
            val s = showFunTerm(hoas)
            println(declarations.reverse.mkString("\n") + "\n" + s)

        }
    }


    sealed trait FunTerm[A]
    case class Atom[A](atom : Term[A]) extends FunTerm[A]
    case class Arrow[A, B](fun : Term[A] => FunTerm[B]) extends FunTerm[A => B]
    
    implicit def atom[A](a : Term[A]) : FunTerm[A] = Atom(a)
    implicit def arrow[A, B, C](f : Term[A] => B) (implicit toFunTerm : B => FunTerm[C]) : FunTerm[A => C] = {
        def f2(t: Term[A]) : FunTerm[C] = toFunTerm(f(t))
        Arrow(f2)
    }

    var declarations = List[String]()

    def showFunTerm[A](term : FunTerm[A]) : String = term match {
        case Atom(atom) => showUntyped(atom.untyped)
        case Arrow(fun) =>
            val i = declarations.length
            val variable = s"x$i"
            declarations ::= variable
            s"$variable -> ${showFunTerm(fun(Term(BuiltIn(variable))))}"
    }

    def showUntyped(u : Untyped) : String = u match {
        case Constant(n) =>
            // We need the decimal point to denote a float.
            // Scala JS will not generate this for integers.
            // This is not a problem in JVM
            val s = n.toString
            if(s.contains('.')) s
            else s + ".0";
        case Bind(variableType, argument, body) =>
            val a = showUntyped(argument)
            val i = declarations.length
            val declaration = s"    $variableType v$i = $a;\n"
            declarations ::= declaration
            showUntyped(body(Variable(i)))
        case Variable(n) => s"v$n"
        case Field(label, target) => s"${showUntyped(target)}.$label"
        case If(condition, whenTrue, whenFalse) => s"(${showUntyped(condition)} ? ${showUntyped(whenTrue)} : ${showUntyped(whenFalse)})"
        case BuiltIn(name) => name
        case Prefix(operator, right) => s"($operator${showUntyped(right)})"
        case Infix(operator, left, right) => s"(${showUntyped(left)} $operator ${showUntyped(right)})"
        case Call(name, arguments) => s"$name(${arguments.map(showUntyped).mkString(", ")})"
    }



}
