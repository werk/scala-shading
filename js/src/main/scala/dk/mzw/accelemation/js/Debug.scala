package dk.mzw.accelemation.js

object Debug {
    def printTime[A](action : => A): Unit = {
        val t1 = System.currentTimeMillis()
        val result = action
        val dt = System.currentTimeMillis() - t1

    }
}
