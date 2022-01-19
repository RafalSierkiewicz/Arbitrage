val currentExchange = Array.ofDim[Double](4, 4)
val fillCurrentExchange = fillRow(currentExchange) _

fillCurrentExchange(0, Seq(1.0, 0.7779, 102.459, 0.0083))
fillCurrentExchange(1, Seq(1.2851, 1.0, 131.711, 0.01125))
fillCurrentExchange(2, Seq(0.0098, 0.0075, 1.0, 0.0000811))
fillCurrentExchange(3, Seq(115.65, 88.8499, 12325.44, 1))

def loop(array: Array[Array[Double]], max: Int = 1): Unit = {
  for (i <- 0 to array.length) {
    for (j <- 0 to array(i).length) {
      // Check if we are not on the diagonal
      if (i != j) {}
      loop(array.slice(j, array.length))
    }
  }
}

def fillRow(array: Array[Array[Double]])(idx: Int, args: Double*) = {
  List.tabulate(args.length - 1)(_ + 1).foreach(j => array(idx)(j) = args(j))
}
