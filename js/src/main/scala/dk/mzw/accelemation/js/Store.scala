package dk.mzw.accelemation.js

import dk.mzw.accelemation.js.BuildOrder.Id

trait Store {
    def list(onSuccess : Seq[(Id, BuildOrder)] => Unit, onError : String => Unit)
    def put(name : String, build : BuildOrder, onSuccess : Id => Unit, onError : String => Unit)
}

class LocalStore(var userId : Option[String]) extends Store {

    import org.scalajs.dom

    val prefix = "animation/"

    override def list(onSuccess : Seq[(Id, BuildOrder)] => Unit, onError : String => Unit) : Unit = {
        val result = for {
            i <- 0 until dom.localStorage.length
            key = dom.localStorage.key(i)
            if key.startsWith(prefix)
            Array(userId, keyName) = key.drop(prefix.length).split("/")
            value = dom.localStorage.getItem(key)
        } yield Id(userId, keyName) -> BuildOrder.readBuildOrder(value)
        dom.setTimeout(() => onSuccess(result), 100)
    }

    override def put(name : String, build : BuildOrder, onSuccess : Id => Unit, onError : String => Unit) : Unit = {
        userId match {
            case None => dom.setTimeout(() => onError("Not signed in."), 100)
            case Some(actualUserId) =>
                val keyName = "animation-" + System.currentTimeMillis()
                dom.localStorage.setItem(prefix + actualUserId + "/" + keyName, BuildOrder.show(build))
                dom.setTimeout(() => onSuccess(Id(actualUserId, keyName)), 100)
        }
    }

}
