import util.control.Breaks._

class Generateur extends Constantes {

  var L: List[Int] = Nil
  var LL: List[List[Int]] = Nil
  var lesCoups: List[Coups] = Nil
  var E: Array[Int] = E0

  plateau.foreach { O =>
    dir.foreach { d =>
      var X = O + d
      while (E(X) != OUT) {
        L :+= X
        X += d
      }
      if (L.nonEmpty)
        LL ::= L
      L = Nil
    }
    TLL(O) = LL
    LL = Nil
  }

  def comptage(): Unit = {
    comptage(E)
  }

  def comptage(E: Array[Int]): Unit = {
    var lc: List[Int] = Nil
    plateau foreach { O =>
      TLL(O) foreach { l =>
        breakable {
          var count = 0
          l foreach {
            c =>
              E(c) match {
                case `opcolor` =>
                  count += 1
                  lc :+= c
                case `vide` =>
                  break
                case `color` =>
                  cpt(O) += count
                  break
              }
          }
        }

      }
      if (cpt(O) != 0) {
        var m = new Coups(O, cpt(O), simul(O, E.clone(), lc))
        fplateau(m.pos)
        println
        lesCoups :+= m
      }
      lc = Nil
    }
  }

  def simul(O: Int, e: Array[Int], lc: List[Int]): Array[Int] = {
    e(O) = color
    lc.foreach(c => e(c) = color)
    e
  }

  def fplateau(e: Array[Int]): Unit = {
    for (j <- K) {
      for (i <- K) {
        e(i + n * j) match {
          case 0 =>
            //if (cpt(i + n * j) == 0) print("-") else print(".")
            print("-")
          case 1 => print("O")
          case -1 => print("X")
        }
      }
      println
    }
  }

  def aplateau(): Unit = {
    lesCoups.foreach { coups =>
      E = coups.pos
      for (j <- K) {
        for (i <- K) {
          E(i + n * j) match {
            case 0 => if (cpt(i + n * j) == 0) print("-") else print(".")
            case 1 => print("O")
            case -1 => print("X")
          }
        }
        println
      }
      println
    }
  }

  // println(lesCoups)
}