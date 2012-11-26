package nnetworks

import com.twitter.util.Config

trait NetworkConfig extends Config[Base]{
  var network = required[AbstractNetwork]
  var training = required[List[(List[Double], List[Double])]]
  def apply  = new Base(network, training)


  def random(a: Double, b: Double):Double = math.random*(b-a)+1
  def RLayers(inputs: Int, layers: List[((Double) => Double, Int, Double, Double)]):List[Layer] =
    layers.foldLeft((List[Layer](), inputs))((acc, layer) => acc match {
      case (current_layers, current_inputs) => layer match {
        case (activation, count, a, b) => (new Layer(activation, ((1 to count).map((_) => (0 to inputs).map((_) => random(a, b)).toList )).toList) :: current_layers , count)
      }
    })._1.reverse

}