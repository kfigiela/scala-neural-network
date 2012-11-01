package nnetworks

import com.twitter.util.Config

trait NetworkConfig extends Config[Base]{
  var network = required[Network]
  var training = required[List[(List[Double], List[Double])]]
  
  def apply() = new Base(network, training)
}
