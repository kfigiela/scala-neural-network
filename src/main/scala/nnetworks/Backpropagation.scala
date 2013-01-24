package nnetworks

object Backpropagation {
    def train(network: Network, bias:Boolean, learningSet: List[(List[Double], List[Double])], alpha: Double, momentum: Double, maxSteps: Integer = 10000, maxError: Double = 0.05):Double = {
      var totalError = 1.0;
      var step = 0;
      val offset = if (bias) 1 else 0
      while(totalError > maxError && step < maxSteps) {
        step += 1
        var error = 0.0;
        learningSet.foreach((item) => item match {
          case (input, expectedOutput)  => {
            val output = network(input)

            error += scala.math.sqrt((output, expectedOutput).zipped.map( -_+_ ).map( (a) => a*a ).reduce(_+_))/expectedOutput.length

            network.layers.last.delta = (output, expectedOutput).zipped.map( -_+_ ).toList

            (1 until network.layers.length).reverse.foreach((i) => {
              val lo = network.layers(i)
              val li = network.layers(i-1)

              val outWeights = (0 until li.neurons.length+offset).map( (j) => lo.neurons.map( (n) => n(j) ) ).toList

              li.delta = outWeights.map( (ow) => (ow, lo.delta).zipped.map((ow,d) => ow*d).reduce(_+_)).toList
            })

            network.layers.foreach((l) => {
              l.update = (l.update, l.delta, l.calculatedInput).zipped.map((neuron, d, cinput) => {
                (neuron, l.previousInput).zipped.map((weight, input) => momentum*weight + alpha * d * l.activationD(cinput) * input).toList
              }).toList
              l.neurons = (l.neurons, l.update).zipped.map((neuron, update) => {
                (neuron, update).zipped.map((weight, update) => weight + update).toList
              }).toList
            })
        }})
        totalError = error/learningSet.length
      }
      println(step)
      println(totalError)
      totalError
    }
}

object BPMain {
  def or {
    println("OR")

    val trainingSet = List(List(0., 0.), List(0., 1.), List(1., 0.), List(1., 1.))
    val teacherSet = List(List(0.), List(1.), List(1.), List(1.))

    val layer  = new Layer(ActivationFunctions.sigmoid, ActivationFunctions.sigmoidD, List(0.1 :: 0.2 :: 0.3 :: Nil, 0.5 :: 0.6 :: 0.7 :: Nil), true)
    val layer2 = new Layer(ActivationFunctions.sigmoid, ActivationFunctions.sigmoidD, List(0.2 :: 0.4 :: 0.6 :: Nil), true)
    val net = new Network(layer, layer2)

    println(net.layers)

    (trainingSet,teacherSet).zipped.foreach((i,o) => println("%s = %s should be %s ".format(i,net(i),o)))


    Backpropagation.train(net, true, (trainingSet, teacherSet).zipped.toList, 0.2, 0.6)

    println(net.layers)
    (trainingSet,teacherSet).zipped.foreach((i,o) => println("%s = %s should be %s ".format(i,net(i),o)))
  }


  def ikonki {
    println("Ikonki")
    val training = List(

       (0. :: 0. :: 0. ::
        1. :: 0. :: 1. ::
        0. :: 0. :: 0. :: Nil, 1. :: 0. :: 0. :: Nil),

       (0. :: 0. :: 0. ::
        0. :: 1. :: 0. ::
        0. :: 0. :: 0. :: Nil, 0. :: 1. :: 0. :: Nil),

       (0. :: 0. :: 0. ::
        1. :: 1. :: 1. ::
        0. :: 0. :: 0. :: Nil, 0. :: 0. :: 1. :: Nil)
    )

    val layer = new Layer(ActivationFunctions.sigmoid, ActivationFunctions.sigmoidD,
      List(
        math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: Nil,
        math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: Nil,
        math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: math.random :: Nil
      )
      ,true)
    val net = new Network(layer)

    println(net.layers)

    training.foreach((i) => println("%s = %s should be %s ".format(i._1,net(i._1),i._2)))


    Backpropagation.train(net, true, training.toList, 0.2, 0.6)

    println(net.layers)
    training.foreach((i) => println("%s = %s should be %s ".format(i._1,net(i._1),i._2)))

  }

  def xor(learningRate: Double, momentum: Double, weights1: List[List[Double]], weights2: List[List[Double]]) {
    println("XOR")

    val trainingSet = List(List(0., 0.), List(0., 1.), List(1., 0.), List(1., 1.))
    val teacherSet = List(List(0.), List(1.), List(1.), List(0.))

    val layer  = new Layer(ActivationFunctions.sigmoid, ActivationFunctions.sigmoidD, weights1, true)
    val layer2 = new Layer(ActivationFunctions.sigmoid, ActivationFunctions.sigmoidD, weights2, true)
    val net = new Network(layer, layer2)

    println(net.layers)

//    (trainingSet,teacherSet).zipped.foreach((i,o) => println("%s = %s should be %s ".format(i,net(i),o)))


    Backpropagation.train(net, true, (trainingSet, teacherSet).zipped.toList, learningRate, momentum)

    println(net.layers)
    (trainingSet,teacherSet).zipped.foreach((i,o) => println("%s = %s should be %s ".format(i,net(i),o)))
  }

  def trixor {
    println("TRIXOR")

    val trainingSet = List(List(0., 0., 0.), List(0., 0., 1.), List(0., 1., 0.), List(0., 1., 1.), List(1., 0., 0.), List(1., 0., 1.), List(1., 1., 0.), List(1., 1., 1.))
    val teacherSet = trainingSet.map((l) => l.reduce((_.toInt^_.toInt)).toDouble :: Nil).toList
    println(teacherSet)

    val layer  = new Layer(ActivationFunctions.sigmoid, ActivationFunctions.sigmoidD,
      (0 until 20).toList.map((_) => List(math.random, math.random, math.random, math.random)).toList
      , true)
    val layer2 = new Layer(ActivationFunctions.sigmoid, ActivationFunctions.sigmoidD,
        List(List(math.random, math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random,  math.random)),
      true)
    val net = new Network(layer, layer2)

//    println(net.layers)

    Backpropagation.train(net, true, (trainingSet, teacherSet).zipped.toList, 0.9, 0.6)

//    println(net.layers)
//    (trainingSet,teacherSet).zipped.foreach((i,o) => println("%s = %s should be %s ".format(i,net(i),o)))
  }


  def main(args: Array[String]) {
//    or

//    ikonki
//
    xor(0.2, 0.2, List(0.1 :: 0.2 :: 0.3 :: Nil, 0.5 :: 0.6 :: 0.7 :: Nil), List(0.2 :: 0.4 :: 0.6 :: Nil))
    xor(0.2, 0.2, List(0.9 :: 0.5 :: 0.8 :: Nil, 0.5 :: 0.2 :: 0.9 :: Nil), List(0.4 :: 0.2 :: 0.7 :: Nil))
    xor(0.2, 0.2, List(0.0 :: -0.5 :: -0.8 :: Nil, -0.5 :: 0.2 :: -0.9 :: Nil), List(0.4 :: -0.2 :: 0.7 :: Nil))

    xor(0.3, 0.4, List(0.1 :: 0.2 :: 0.3 :: Nil, 0.5 :: 0.6 :: 0.7 :: Nil), List(0.2 :: 0.4 :: 0.6 :: Nil))
    xor(0.3, 0.4, List(0.9 :: 0.5 :: 0.8 :: Nil, 0.5 :: 0.2 :: 0.9 :: Nil), List(0.4 :: 0.2 :: 0.7 :: Nil))
    xor(0.3, 0.4, List(0.0 :: -0.5 :: -0.8 :: Nil, -0.5 :: 0.2 :: -0.9 :: Nil), List(0.4 :: -0.2 :: 0.7 :: Nil))

    xor(0.6, 0.5, List(0.1 :: 0.2 :: 0.3 :: Nil, 0.5 :: 0.6 :: 0.7 :: Nil), List(0.2 :: 0.4 :: 0.6 :: Nil))
    xor(0.6, 0.5, List(0.9 :: 0.5 :: 0.8 :: Nil, 0.5 :: 0.2 :: 0.9 :: Nil), List(0.4 :: 0.2 :: 0.7 :: Nil))
    xor(0.6, 0.5, List(0.0 :: -0.5 :: -0.8 :: Nil, -0.5 :: 0.2 :: -0.9 :: Nil), List(0.4 :: -0.2 :: 0.7 :: Nil))

    xor(0.8, 0.6, List(0.9 :: 0.5 :: 0.8 :: Nil, 0.5 :: 0.2 :: 0.9 :: Nil), List(0.4 :: 0.2 :: 0.7 :: Nil))
    xor(0.8, 0.6, List(0.1 :: 0.2 :: 0.3 :: Nil, 0.5 :: 0.6 :: 0.7 :: Nil), List(0.2 :: 0.4 :: 0.6 :: Nil))
    xor(0.8, 0.6, List(0.0 :: -0.5 :: -0.8 :: Nil, -0.5 :: 0.2 :: -0.9 :: Nil), List(0.4 :: -0.2 :: 0.7 :: Nil))

//    trixor
  }
}
