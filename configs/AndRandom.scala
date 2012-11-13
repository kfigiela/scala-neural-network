import nnetworks._
import nnetworks.ActivationFunctions._

new NetworkConfig {
  training = List(
    (0. :: 0. :: Nil, 0. :: Nil),
    (0. :: 1. :: Nil, 0. :: Nil),
    (1. :: 0. :: Nil, 0. :: Nil),
    (1. :: 1. :: Nil, 1. :: Nil)
  )
  
  network = Network(
    RLayers(2, (sigmoid _, 2, -10.0, -1.0) :: Nil): _*
  )
}
