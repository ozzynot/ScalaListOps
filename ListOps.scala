object ListOps {

  def sum(xs: List[Int]): Int = {
    if (xs.isEmpty) 0
    else xs.head + sum(xs.tail)
  }

  def max(xs: List[Int]): Int = {
    if (xs.isEmpty) throw new NoSuchElementException("empty list")
    else if (xs.tail.isEmpty) xs.head
    else {
      val tailMax = max(xs.tail)
      if (xs.head > tailMax) xs.head else tailMax
    }
  }

  def sum2(xs: List[Int]): Int = xs match {
    case Nil => 0
    case head :: tail => head + sum2(tail)
  }

  def max2(xs: List[Int]): Int = xs match {
    case Nil => throw new NoSuchElementException("empty list")
    case head :: Nil => head
    case head :: tail =>
      val tailMax = max2(tail)
      if (head > tailMax) head else tailMax
  }
}
