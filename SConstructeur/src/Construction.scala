object Construction extends App {

  val n = 10

  // Table
  for (i <- List.range(0, n)) {
    for (j <- List.range(0, n)) {
      print(i + n * j + ",")
    }
    println
  }

  println()

  // Plateau
  for (i <- List.range(1, n - 1)) {
    for (j <- List.range(1, n - 1)) {
      print(i + n * j + ",")
    }
    println
  }
}
