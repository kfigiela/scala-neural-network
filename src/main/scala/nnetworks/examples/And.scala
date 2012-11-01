package nnetworks.examples

import nnetworks.ActivationFunctions._
import nnetworks.Network
import nnetworks.Layer

abstract class AndNetwork extends Base {
  val training = List(
    (0. :: 0. :: Nil, 0. :: Nil),
    (0. :: 1. :: Nil, 0. :: Nil),
    (1. :: 0. :: Nil, 0. :: Nil),
    (1. :: 1. :: Nil, 1. :: Nil)
  )
}

object And extends AndNetwork {
  val network = Network(
    Layer(sigmoid, List(
      -30.0 :: 20.0 :: 20.0 :: Nil
    ))
  )
}

object AndRandom extends AndNetwork {
  val network = Network(
    Layer(sigmoid, List(
       math.random :: math.random :: math.random :: Nil
    ))
    )
}
