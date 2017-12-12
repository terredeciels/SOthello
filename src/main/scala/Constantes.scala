trait Constantes {
  val n = 10
  val dim: Int = n * n - 1
  val OUT = 2
  val color: Int = -1
  val opcolor = 1
  val vide = 0
  val cpt = new Array[Int](dim)
  val dir = List(n, n + 1, 1, 1 - n, -n, -n - 1, -1, -1 + n)
  val I: List[Int] = List.range(0, n)
  val K: List[Int] = List.range(1, n - 1)
  val TLL = new Array[List[List[Int]]](n * n)

  val plateau = List(11, 21, 31, 41, 51, 61, 71, 81,
    12, 22, 32, 42, 52, 62, 72, 82,
    13, 23, 33, 43, 53, 63, 73, 83,
    14, 24, 34, 44, 54, 64, 74, 84,
    15, 25, 35, 45, 55, 65, 75, 85,
    16, 26, 36, 46, 56, 66, 76, 86,
    17, 27, 37, 47, 57, 67, 77, 87,
    18, 28, 38, 48, 58, 68, 78, 88)

  val table = List(0, 10, 20, 30, 40, 50, 60, 70, 80, 90,
    1, 11, 21, 31, 41, 51, 61, 71, 81, 91,
    2, 12, 22, 32, 42, 52, 62, 72, 82, 92,
    3, 13, 23, 33, 43, 53, 63, 73, 83, 93,
    4, 14, 24, 34, 44, 54, 64, 74, 84, 94,
    5, 15, 25, 35, 45, 55, 65, 75, 85, 95,
    6, 16, 26, 36, 46, 56, 66, 76, 86, 96,
    7, 17, 27, 37, 47, 57, 67, 77, 87, 97,
    8, 18, 28, 38, 48, 58, 68, 78, 88, 98,
    9, 19, 29, 39, 49, 59, 69, 79, 89, 99)

  val E0 = Array(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 0, 0, 0, 0, 0, 0, 0, 0, 2,
    2, 0, 0, 0, 0, 0, 0, 0, 0, 2,
    2, 0, 0, 0, 0, 0, 0, 0, 0, 2,
    2, 0, 0, 0, 1, -1, 0, 0, 0, 2,
    2, 0, 0, 0, -1, 1, 0, 0, 0, 2,
    2, 0, 0, 0, 0, 0, 0, 0, 0, 2,
    2, 0, 0, 0, 0, 0, 0, 0, 0, 2,
    2, 0, 0, 0, 0, 0, 0, 0, 0, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
}