object exercise {

  def product(f: Int => Int)(a: Int, b:Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def factorial(a: Int, b: Int): Int = {
    product(x => x)(a, b)
  }

  def mapReduce(f: Int => Int, reduce: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else reduce(f(a), mapReduce(f, reduce, zero)(a+1, b))
  }

  def newProduce(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x, y) => x * y, 1)(a, b)

  def main(args: Array[String]) = {
    val a = product(x => x * x)(3, 4)
    println(a)

    val b = newProduce(x => x * x)(3, 4)
    println(b)

    val fac = factorial(1, 4)
    println(fac)

  }
}
