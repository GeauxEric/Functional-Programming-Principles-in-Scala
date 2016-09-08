import java.util.Scanner
import java.io.File

object maps {
  def main(args: Array[String]) = {
    // Set up a map of prices for a number of gizmos that you covet. Then produce a
    // second map with the same keys and the prices at a 10 percent discount
    val gizmos = Map("geauxdock" -> 10, "xcms" -> 5, "stackflows" -> 15)
    val discounted_gizmos = for ((k, v) <- gizmos) yield (k, 0.9 * v)

    val weekdays = scala.collection.mutable.LinkedHashMap("Monday" -> java.util.Calendar.MONDAY)
    weekdays += ("Wednesday" -> java.util.Calendar.WEDNESDAY)
    weekdays += ("Tuesday" -> java.util.Calendar.TUESDAY)
    weekdays += ("Thursday" -> java.util.Calendar.THURSDAY)

    for ((k, v) <- weekdays) yield (k, v)

    val a = Array[Int](1,3,2,6,1,4,5,6,88)
    val b = a.sorted
    val c = b.reverse
    val d = b.zip(c)
    val e = d(0)

    val f = "hello".zip("world")
  }
}
