package nnetworks

object Backpropagation {
    def train(network: Network, bias:Boolean, learningSet: List[(List[Double], List[Double])], alpha: Double, momentum: Double):Unit = {
      val offset = if (bias) 1 else 0
      (0 to 30000).toList.foreach((step) => {
        var error = 0.0;
        learningSet.foreach((item) => item match {
          case (input, expectedOutput)  => {
            val output = network(input)

            error += scala.math.sqrt((output, expectedOutput).zipped.map( -_+_ ).map( (a) => a*a ).reduce(_+_))/expectedOutput.length
//            println(output)


            network.layers.last.delta = (output, expectedOutput).zipped.map( -_+_ ).toList

            (1 until network.layers.length).reverse.foreach((i) => {
              val lo = network.layers(i)
              val li = network.layers(i-1)

              val outWeights = (0 until li.neurons.length).map( (j) => lo.neurons.map( (n) => n(j+offset) ) )

              li.delta = (outWeights, lo.delta).zipped.map( (ow, d) => ow.map (_*d).reduce(_+_)).toList
            })

            network.layers.foreach((l) => {
              l.neurons = (l.neurons, l.delta, l.cinput).zipped.map((neuron, d, cinput) => {
                (neuron, l.ainput).zipped.map((weight, input) => weight + alpha * d * l.activationD(cinput) * input).toList
              }).toList
            })

//            println(network.layers)
//            println(network.layers.map(_.delta))
        }})
//        println(error/learningSet.length)
      })

    }
}

object BPMain {
  def main(args: Array[String]) {
    val trainingSet = List(List(0., 0.), List(0., 1.), List(1., 0.), List(1., 1.))
    val teacherSet = List(List(0.), List(1.), List(1.), List(1.))

    val layer = new Layer(ActivationFunctions.sigmoid, ActivationFunctions.sigmoidD, List(0.1 :: 0.2 :: 0.3 :: Nil, 0.5 :: 0.6 :: 0.7 :: Nil), true)
    val layer2 = new Layer(ActivationFunctions.sigmoid, ActivationFunctions.sigmoidD, List(0.2 :: 0.4 :: 0.6 :: Nil), true)
    val net = new Network(layer, layer2)

    println(net.layers)
       println(net(List(0., 0.)))
	println(net(List(1., 0.)))
	println(net(List(0., 1.)))
	println(net(List(1., 1.)))


    Backpropagation.train(net, true, (trainingSet, teacherSet).zipped.toList, 0.2, 0.6)

    println(net.layers)
       println(net(List(0., 0.)))
	println(net(List(1., 0.)))
	println(net(List(0., 1.)))
	println(net(List(1., 1.)))

}
}
