import nnetworks._
import nnetworks.ActivationFunctions._

new NetworkConfig {
  val inputs = List(
     1. :: 1. :: 1. ::
      0. :: 0. :: 1. ::
      1. :: 1. :: 1. :: Nil
    ,
     0. :: 1. :: 0. ::
      1. :: 0. :: 1. ::
      0. :: 1. :: 0. :: Nil
    ,
     1. :: 1. :: 1. ::
      0. :: 0. :: 0. ::
      1. :: 1. :: 1. :: Nil
    ,
     1. :: 0. :: 1. ::
      0. :: 1. :: 0. ::
      0. :: 1. :: 0. :: Nil
  )

  training = inputs.map((_, Nil))

  network = Network(
    KohonenTraining(
      KohonenLayer(
        (0 until 4).map( (_) => List(math.random, math.random, math.random, math.random, math.random, math.random, math.random, math.random, math.random)).toList
      ),
      inputs, List((8000, 0.06, 1.0, 1, 3), (8000, 0.03, 0.5, 1, 2), (8000, 0.015, 0.25, 1, 1), (8000, 0.0075, 0.125, 1, 0))
//      , training, 10000, 0.03, 0.5, 0.1)
    )
  )
}
