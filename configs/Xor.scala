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
      7.281 :: -4.868 :: -4.917 :: Nil,
      2.798 :: -6.651 :: -6.959 :: Nil
    )),
    Layer(sigmoid, List(
      -4.848 :: 10.187 :: -10.275 :: Nil
    ))
  )
}
