package nnetworks

trait AbstractNetwork {
  def apply(input: List[Double]): List[Double]
}

object Helpers {
  def normalizationFactor(numbers:List[Double]):Double = math.sqrt(numbers.map(x=>x*x).sum)
}

case class Layer(val activation: (Double) => Double, var neurons: List[List[Double]], val bias:Boolean = true) {
  def apply(input: List[Double]): List[Double] =
    if (bias)
      neurons map ((neuron) => activation(((1.0 :: input), neuron).zipped.map(_*_).sum))
    else
      neurons map ((neuron) => activation((input, neuron).zipped.map(_*_).sum))
}

case class Network(layers: Layer*) extends AbstractNetwork{
  def apply(input: List[Double]): List[Double] = layers.foldLeft(input)((acc, layer) => layer(acc))
}
