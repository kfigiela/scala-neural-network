package nnetworks


case class Base(network: AbstractNetwork, training: List[(List[Double], List[Double])]) {
  def main(args: List[String]) {
    if (args.length == 2) {
      println("f(%s) = %s".format(args.mkString(", "), network(args.toList.map(_.toDouble)).mkString("(", ", ", ")")))
    } else {
      training.foreach((data) => data match {
        case (inputs, expected) => {
          val results = network(inputs)
//          val errors = (expected, results).zipped.map(_-_)
          println("N(%s) = (%s) expected (%s)".format(inputs.mkString(", "), results.mkString(", "), expected.toString()))
        }
      })
    }
  }
}

