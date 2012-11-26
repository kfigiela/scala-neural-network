package nnetworks

trait AbstractNetwork {
  def apply(input: List[Double]): List[Double]
}

object Helpers {
  def normalizationFactor(numbers:List[Double]):Double = math.sqrt(numbers.map(x=>x*x).reduce(_+_))
}

case class Layer(activation: (Double) => Double, neurons: List[List[Double]]) {
  def apply(input: List[Double]): List[Double] = neurons map ((neuron) => activation(((1.0 :: input), neuron).zipped.map(_*_).reduce(_+_)))
}

case class Network(layers: Layer*) extends AbstractNetwork{
  def apply(input: List[Double]): List[Double] = layers.foldLeft(input)((acc, layer) => layer(acc))
}
