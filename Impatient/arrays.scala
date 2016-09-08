// Arrays

import java.util.TimeZone.getAvailableIDs
import java.awt.datatransfer._


object ArraysExercises {
  def main(args: Array[String]) = {

    // Write a code snippet that produces all values from an array with duplicates
    // removed
    def removeDup(a: Array[Int]) : Array[Int] = {
      a.distinct
    }

    val a = Array(1,2,2,3,4,5,6,6)
    val b = removeDup(a)

    val ids = getAvailableIDs()

    def dropAmerica(s: String) : String = {
      if (s.startsWith("America/")) s.drop(8)
      else s
    }

    val newids = ids.map(dropAmerica)

    val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]

    val buffer = flavors.getNativesForFlavor(DataFlavor.imageFlavor)

  }
}

