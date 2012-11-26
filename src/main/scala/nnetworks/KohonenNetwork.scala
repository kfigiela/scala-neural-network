package nnetworks


case class KohonenNetwork(val weights: List[List[Double]], val conscience: List[Double]) extends AbstractNetwork {
  def calculateOutputs(inputs: List[Double]) = {
    val normalizationFactor = Helpers.normalizationFactor(inputs)
    val normalizedInputs = inputs.map( x => (x/normalizationFactor)*2.0-1.0)
    weights.map( neuron => (normalizedInputs, neuron).zipped.map(_*_).reduce(_+_) ).map ( x => (x / normalizationFactor)*2.0 - 1.0 )
  }

  def closest(inputs:List[Double], beta: Double):Int = {
    (weights.map(neuron => (neuron, inputs).zipped.map((x,y) => (math.abs(x-y))).sum).zipWithIndex, conscience).zipped.filter((w, c) => c > beta)._1.min._2
  }

  def apply(inputs: List[Double]): List[Double] = List(closest(inputs, 0.0))

  def size = weights.length
}

object KohonenTraining {
  def distance(a: List[Double], b: List[Double]):Double = math.sqrt((a,b).zipped.map((a,b) => math.pow(a-b, 2)).sum)
  def gaussian(weights: List[Double], winner: List[Double], gamma: Double) =
  //    if (weights == winner) 1.0 else 0.0
    math.exp(-distance(weights, winner)/gamma)
  def saturate(x: Double) = if (x > 1.0) 1.0 else if (x < 0.0) 0.0 else x

  def apply(network: KohonenNetwork, data: List[(List[Double], List[Double])], epochs:Int, alfa: Double, gamma: Double, beta: Double) = {
    (0 to epochs).foldLeft(network)( (network, _) =>
      data.foldLeft(network)((network, item:(List[Double],List[Double])) => item match {
        case (vector, _) => {
          val result = network.closest(vector, beta)
          val winnerWeights = network.weights(result)
          val newWeights = network.weights.map( (neuron) =>
            (neuron, vector).zipped.map(
              (x, y) => x + gaussian(neuron, winnerWeights, gamma)*alfa*(y-x)
            ).toList
          )
          val newConscience = network.conscience.zipWithIndex.map( (x) => saturate(if(x._2 != result) x._1 + 1.0/network.size else x._1 - beta))
          println(newWeights)
          KohonenNetwork(newWeights, newConscience)
        }
      }
      )
    )
  }
}