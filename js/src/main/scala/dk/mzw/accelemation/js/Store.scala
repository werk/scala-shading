package dk.mzw.accelemation.js

import dk.mzw.accelemation.js.BuildOrder.Id

trait Store {
    def list(onSuccess : Seq[(Id, BuildOrder)] => Unit, onError : String => Unit)
    def put(name : String, build : BuildOrder, onSuccess : Id => Unit, onError : String => Unit)
}
