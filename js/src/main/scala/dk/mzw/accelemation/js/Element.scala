package dk.mzw.accelemation.js

import org.scalajs.dom.document

object Element {

    def node(tag : String) = {
        document.createElement(tag)
    }


}
