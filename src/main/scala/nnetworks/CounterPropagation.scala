package nnetworks


abstract class Teacher(val learnRate: Double) {
  def teach(layer: Layer, examples: List[(List[Double], List[Double])]) = null

  def psp(weights: List[Double], inputs: List[Double]) = {
    (for {(x, y) <- weights zip inputs} yield x * y).sum
  }
  def numericDerivative(function: Double => Double, x: Double) = {
    val epsilon = 0.0001
    (function(x+epsilon) - function(x-epsilon)) / (2 * epsilon)
  }
}

class DeltaRuleTeacher(val learnRatey: Double) extends Teacher(learnRatey) {
  override def teach(layer: Layer, examples: List[(List[Double], List[Double])]) = {
    for {(input, targetOutput) <- examples} yield {
      val layerOutput = layer.apply(input)
      assert(layerOutput.length == targetOutput.length && targetOutput.length == layer.neurons.length)
      val newLayer = for {(targetValue, layerValue, neuronWeights) <- (targetOutput, layerOutput, layer.neurons).zipped.toList} yield {
        val weightedSum = psp(neuronWeights, input)
        val derivative = numericDerivative(layer.activation, weightedSum)
        val newWeights = for {(inputValue, weight) <- input zip neuronWeights} yield {
          val delta = learnRate * (targetValue - layerValue) * derivative * inputValue
          weight + delta
        }
        newWeights
      }
      layer.neurons = newLayer
    }
    null
  }
}

class WidrowHoffTeacher(val learnRatex: Double) extends DeltaRuleTeacher(learnRatex) {
  override def numericDerivative(function: Double => Double, x: Double) = 1
}