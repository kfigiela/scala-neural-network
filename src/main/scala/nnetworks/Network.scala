package nnetworks

trait AbstractNetwork {
  def apply(input: List[Double]): List[Double]
}

object Helpers {
  def normalizationFactor(numbers:List[Double]):Double = math.sqrt(numbers.map(x=>x*x).sum)
}

case class Layer(val activation: (Double) => Double, val activationD: (Double) => Double, var neurons: List[List[Double]], val bias:Boolean = true) {

  var calculatedInput:List[Double] = List()
  var previousInput:List[Double] = List()
  var delta :List[Double] = List()
  var update:List[List[Double]] = neurons.map(_.map((x) => 0.0)).toList

  def apply(input: List[Double]): List[Double] = {

    if (bias) {
      previousInput = 1.0 :: input
      calculatedInput = neurons map ((neuron) => ((1.0 :: input), neuron).zipped.map(_*_).sum)
      neurons map ((neuron) => activation(((1.0 :: input), neuron).zipped.map(_*_).sum))
    } else {
      previousInput = input
      calculatedInput = neurons map ((neuron) => (input, neuron).zipped.map(_*_).sum)
      neurons map ((neuron) => activation((input, neuron).zipped.map(_*_).sum))
    }
  }
}

case class Network(layers: Layer*) extends AbstractNetwork{
  def apply(input: List[Double]): List[Double] = layers.foldLeft(input)((acc, layer) => layer(acc))
}
