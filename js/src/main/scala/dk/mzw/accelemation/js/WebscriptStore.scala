package dk.mzw.accelemation.js

import dk.mzw.accelemation.js.BuildOrder.Id
import org.scalajs.dom

import scala.collection.mutable.ListBuffer
import scala.scalajs.js.Any

class WebscriptStore(var userId : Option[String]) extends Store {

    val webscriptPrefix = "http://glsl.webscript.io"

    /*

        local loadedText = storage.animations
        if loadedText == nil then loadedText = '' end

        return loadedText,
          {['Content-Type']='text/plain'}

     */

    override def list(onSuccess : (Seq[(Id, BuildOrder)]) => Unit, onError : (String) => Unit) : Unit = {
        val xhr = new dom.XMLHttpRequest()
        xhr.open("GET", webscriptPrefix + "/animations", async = true)
        xhr.onload = (e : dom.Event) => {
            if(xhr.readyState == 4) {
                if(xhr.status == 200) {
                    val splits = xhr.responseText.split("(^|\\n)\\s*[meta]\\s*(\\n|$)").filter(_.trim.nonEmpty)
                    val groups = ListBuffer[Seq[String]]()
                    val group = ListBuffer[String]()
                    for(line <- xhr.responseText.lines) {
                        if(line == "[meta]") {
                            if(group.nonEmpty) groups += group.toList
                            group.clear()
                            group += line
                        } else if(line.trim.nonEmpty && group.nonEmpty) {
                            group += line
                        }
                    }
                    if(group.nonEmpty) groups += group.toList
                    val animationTexts = groups.map(_.mkString("\n"))
                    val builds = for(text <- animationTexts) yield {
                        val build = BuildOrder.readBuildOrder(text)
                        build.saved.get.id -> build
                    }
                    onSuccess(builds)
                } else {
                    onError(xhr.status + " " + xhr.statusText)
                }
            }
        }
        xhr.send()
    }

    /*

        assert(request.body ~= nil, 'No animation provided.')
        assert(string.sub(request.body, 1, string.len("[meta]")) == "[meta]", 'Invalid animation.')

        local loadedText = storage.animations
        if loadedText == nil then loadedText = '' end

        storage.animations = loadedText .. "\n" .. request.body

        return 'ok',
          {['Content-Type']='text/plain'}

     */

    override def put(name : String, build : BuildOrder, onSuccess : Id => Unit, onError : (String) => Unit) : Unit = {
        userId match {
            case None => dom.setTimeout(() => onError("Not signed in."), 100)
            case Some(actualUserId) =>
                val keyName = "animation-" + System.currentTimeMillis()
                val id = Id(actualUserId, keyName)
                val xhr = new dom.XMLHttpRequest()
                xhr.open("POST", webscriptPrefix + "/save-animation", async = true)
                xhr.onload = (e : dom.Event) => {
                    if(xhr.readyState == 4) {
                        if(xhr.status == 200) {
                            onSuccess(id)
                        } else {
                            onError(xhr.status + " " + xhr.statusText)
                        }
                    }
                }
                val savedInfo = SavedInfo(id, name)
                val buildText = BuildOrder.show(build.copy(saved = Some(savedInfo)))
                xhr.send(buildText)
        }
    }
}

object WebscriptStore {
    val store = new WebscriptStore(Some("webscript"))
}
