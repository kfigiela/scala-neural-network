import nnetworks._
import nnetworks.ActivationFunctions._

new NetworkConfig {
  training = List(
    ( 0. :: 0. :: 1. ::
      0. :: 0. :: 1. ::
      0. :: 0. :: 1. :: Nil
    , Nil),
    ( 0. :: 1. :: 0. ::
      1. :: 1. :: 1. ::
      0. :: 1. :: 0. :: Nil
    , Nil),
    ( 1. :: 1. :: 1. ::
      1. :: 0. :: 1. ::
      1. :: 1. :: 1. :: Nil
    , Nil),
    ( 1. :: 0. :: 0. ::
      0. :: 1. :: 0. ::
      0. :: 0. :: 1. :: Nil
    , Nil)
  )
  network = KohonenTraining(KohonenNetwork(
    (0 until 4).map( (_) => List(math.random, math.random, math.random, math.random)).toList,
    (0 until 4).map( _ => 1.0).toList
  ), training, 8000, 0.1, 0.25, 0.75)
}
