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
    Layer(sigmoid, List(
      -30.0 :: 20.0 :: 20.0 :: Nil
    ))
  )
}
