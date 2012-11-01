package nnetworks

case class Layer(activation: (Double) => Double, neurons: List[List[Double]]) {
  def apply(input: List[Double]): List[Double] = neurons map ((neuron) => activation(((1.0 :: input), neuron).zipped.map(_*_).reduce(_+_)))
}

case class Network(layers: Layer*){
  def apply(input: List[Double]): List[Double] = layers.foldLeft(input)((acc, layer) => layer(acc))
}

