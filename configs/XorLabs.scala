import nnetworks._
import nnetworks.ActivationFunctions._

new NetworkConfig {
  training = List(
    (0. :: 0. :: Nil, 0. :: Nil),
    (0. :: 1. :: Nil, 1. :: Nil),
    (1. :: 0. :: Nil, 1. :: Nil),
    (1. :: 1. :: Nil, 0. :: Nil)
  )

  network = Network(
    Layer(sigmoid, List(
      2.76 :: -5.38 ::  5.46 :: Nil,
      -2.64 :: -5.06 ::  4.83 :: Nil
    )),
    Layer(sigmoid, List(
      3.34 :: -7.25 :: 7.70 :: Nil
    ))
  )
}
