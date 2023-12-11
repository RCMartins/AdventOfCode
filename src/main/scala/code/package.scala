package object code {

  implicit class TupleExtensions(val tuple: (Int, Int)) extends AnyVal {
    def +(other: (Int, Int)): (Int, Int) = (tuple._1 + other._1, tuple._2 + other._2)
    def *(factor: Int): (Int, Int) = (tuple._1 * factor, tuple._2 * factor)
    def unit: (Int, Int) = {
      val (x, y) = tuple
      (
        if (x == 0) 0 else if (x > 0) 1 else -1,
        if (y == 0) 0 else if (y > 0) 1 else -1,
      )
    }

  }

}
