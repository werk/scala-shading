package dk.mzw.accelemation.js.debug

object ExecutionTime {
    def apply[R](action : => R) : (R, Long) = {
        val begun = System.currentTimeMillis()
        val r = action
        val time = System.currentTimeMillis() - begun
        (r, time)
    }

    def print[R](label : String) (action : => R) : R = {
        val (result, time) = apply(action)
        println(label + ": " + LongTime.pretty(time))
        result
    }

    def only[R](action : => R) : Long = apply(action)._2
}
