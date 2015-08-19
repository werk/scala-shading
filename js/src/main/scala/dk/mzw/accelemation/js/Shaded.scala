package dk.mzw.accelemation.js

import org.scalajs.dom
import scala.scalajs.js

class Shaded(fragmentSourceCode : String, startTime : js.Date) extends js.Object {
    def canvas : dom.Element = js.native
    def dispose() : Unit = js.native
    def resize(width : Int, height : Int) : Unit = js.native
    def step() : Unit = js.native
}
