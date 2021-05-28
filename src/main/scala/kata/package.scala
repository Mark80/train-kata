import scala.annotation.tailrec

package object kata {

  case class Train(repr: String) {

    def fill: Train = {

      val trainBroken = repr.split("::").toList
      val indexOfFirstEmptyCar = trainBroken.indexOf("|____|")
      if(indexOfFirstEmptyCar < 0)
        throw new RuntimeException("empty car not found")
      val (headTrain,tailTrain) = trainBroken.splitAt(indexOfFirstEmptyCar)
      val newRepr = (headTrain ++ ("|^^^^|" :: tailTrain.tail)).mkString("::")

      new Train(newRepr)
    }


    def detachEnd: Train = {
      val trainBroken = repr.split("::")
      val newRepr = trainBroken.take(trainBroken.size - 1).mkString("::")
      new Train(newRepr)
    }

    def detachHead: Train = {
      val trainBroken = repr.split("::")
      val newRepr = trainBroken.tail.mkString("::")
      new Train(newRepr)
    }

  }


  object Train {

    val Locomotive = "<HHHH"
    val ReverseLocomotive = "::HHHH>"
    val PassengerCar = "::|OOOO|"
    val RestaurantCar = "::|hThT|"
    val CargoCar = "::|____|"


    def build(input: String): Train = {
      val wagons = input.toCharArray.toList
      new Train(loop(wagons, ""))
    }

    @tailrec
    private def loop(input: List[Char], acc: String): String = input match {
      case Nil => acc

      case car :: Nil if lastCarIsaLocomotive(acc, car) =>
        acc + ReverseLocomotive

      case car :: _ =>
        val r = car match {
          case 'H' => acc + Locomotive
          case 'R' => acc + RestaurantCar
          case 'C' => acc + CargoCar
          case _ => acc + PassengerCar
        }
        loop(input.tail, r)

    }

    private def lastCarIsaLocomotive(acc: String, car: Char) = {
      acc.nonEmpty && car == 'H'
    }
  }

}
