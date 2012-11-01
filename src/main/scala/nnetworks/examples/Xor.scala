package nnetworks.examples

import nnetworks.ActivationFunctions._
import nnetworks.Network
import nnetworks.Layer

abstract class XorNetwork extends Base {
  val training = List(
    (0. :: 0. :: Nil, 0. :: Nil),
    (0. :: 1. :: Nil, 1. :: Nil),
    (1. :: 0. :: Nil, 1. :: Nil),
    (1. :: 1. :: Nil, 0. :: Nil)
  )
}

object Xor extends XorNetwork {
  val network = Network(
    Layer(sigmoid, List(
      7.281 :: -4.868 :: -4.917 :: Nil,
      2.798 :: -6.651 :: -6.959 :: Nil
    )),
    Layer(sigmoid, List(
      -4.848 :: 10.187 :: -10.275 :: Nil
    ))
  )
}

object XorRandom extends XorNetwork {
  val network = Network(
    Layer(sigmoid, List(
      math.random :: math.random:: math.random :: Nil,
      math.random :: math.random:: math.random :: Nil
    )),
    Layer(sigmoid, List(
      math.random :: math.random:: math.random :: Nil
    ))
  )
}