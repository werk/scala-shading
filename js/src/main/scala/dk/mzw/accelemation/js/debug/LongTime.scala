package dk.mzw.accelemation.js.debug

object LongTime {
    private val second : Long = 1000
    private val minute : Long = 60 * second
    private val hour : Long = 60 * minute
    private val day : Long = 24 * hour
    private val year : Long = 365 * day

    def second(amount : Int) : Long = amount * second
    def minutes(amount : Int) : Long = amount * minute
    def hours(amount : Int) : Long = amount * hour
    def days(amount : Int) : Long = amount * day
    def years(amount : Int) : Long = amount * year

    def pretty(time : Long) : String = {
        if(time > years(2)) formatUnit(time, year, "year")
        else if(time > days(2)) formatUnit(time, day, "day")
        else if(time > hours(2)) formatUnit(time, hour, "hour")
        else if(time > minutes(2)) formatUnit(time, minute, "minute")
        else if(time > second(2)) formatUnit(time, second, "second")
        else time + "ms"
    }

    private def formatUnit(time : Long, unitSize : Long, unitName: String) : String = {
        val amount = (time.toDouble / unitSize).round
        s"$amount $unitName${if(amount == 1) "" else "s"}"
    }

}
