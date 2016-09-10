trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}


def findNthElement[T](l: List[T], n: Int): T = {
  if (l.isEmpty) throw new IndexOutOfBoundsException()
  else if (n == 0) {
    l.head
  }
  else findNthElement(l.tail, n - 1)
}


val l = new Cons(1, new Cons(2, new Cons(3, new Nil)))

val x = findNthElement(l, 2)
