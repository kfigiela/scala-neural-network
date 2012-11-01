import nnetworks._
import nnetworks.ActivationFunctions._

new NetworkConfig {
  training = List(
    (Math :: 0. :: Nil, 0. :: Nil),
    (Math :: 1. :: Nil, 0. :: Nil),
    (Math :: 0. :: Nil, 0. :: Nil),
    (Math :: 1. :: Nil, 1. :: Nil)
  )
  
  network = Network(
    Layer(sigmoid, List(
      math.random :: math.random :: math.random :: Nil
    ))
  )
}
