package nnetworks

object CPMain {
  val inputs =
    List(1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0) ::
    List(0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0) ::
    List(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0) ::
    List(1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0) ::
    List(0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0) ::
    List(0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0) ::
    List(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0) ::
    List(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0) ::
    List(0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0) :: Nil

  var kohonenLay = KohonenLayer(
    (0 until 9).toList.map((x) => (0 until 9).toList.map((y) => math.random))
  )


  var grossbergLay = Layer(ActivationFunctions.id, List(math.random, math.random, math.random, math.random, math.random, math.random, math.random, math.random, math.random) :: List(math.random, math.random, math.random, math.random, math.random, math.random, math.random, math.random, math.random) :: List(math.random, math.random, math.random, math.random, math.random, math.random, math.random, math.random, math.random) :: Nil, false)


  def fmt(v: Any): String = v match {
    case d : Double => "%1.5f" format d
    case i : Int => i.toString
    case _ => throw new IllegalArgumentException
  }

  def printList(list: List[Double]) = list.map("%1.5f".format(_)).mkString(", ")

  def printResults(nn: Network, inputs: List[List[Double]]) {
    println("="*20)
    for (i <- inputs) yield {
      println("Input:  " + printList(i))
      println("Result: " + printList(nn.apply(i)))
      println

    }
  }


  def learnGrossberg(learnRate: Double, epochs: Int) {
    val teacher = new WidrowHoffTeacher(learnRate)
    val firstClass = List(1.0, 0.0, 0.0)
    val secondClass = List(0.0, 1.0, 0.0)
    val thirdClass = List(0.0, 0.0, 1.0)
    val outputs = List(firstClass, firstClass, firstClass, secondClass, secondClass, secondClass, thirdClass, thirdClass, thirdClass)
    val grossInputs = for {input <- inputs} yield kohonenLay.apply(input)

    for {iter <- 1 to epochs} {
      teacher.teach(grossbergLay, grossInputs zip outputs)
    }
  }

  def main(args: Array[String]) {


    KohonenTraining(kohonenLay, inputs, List((8000, 0.06, 1.0, 1, 3), (8000, 0.03, 0.5, 1, 2), (8000, 0.015, 0.25, 1, 1), (8000, 0.0075, 0.125, 1, 0)))
    println(kohonenLay.neurons.map((n) => "N[" + n.map((v) => "%1.2f".format(v)).mkString("  ") + "]").mkString("\n"))
    learnGrossberg(0.1, 50)
    val nn = new Network(kohonenLay, grossbergLay)
    printResults(nn, inputs)
  }
}
