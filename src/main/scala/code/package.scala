package object code {

  implicit class TupleExtensions(val tuple: (Int, Int)) extends AnyVal {
    def +(other: (Int, Int)): (Int, Int) = (tuple._1 + other._1, tuple._2 + other._2)
    def *(factor: Int): (Int, Int) = (tuple._1 * factor, tuple._2 * factor)
  }

}
