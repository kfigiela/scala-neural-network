package nnetworks

import math.abs, math.sqrt

case class KohonenLayer (n: List[List[Double]]) extends Layer (ActivationFunctions.id, ActivationFunctions.idD, n) {
  def this(inputs : Int, outputs : Int) = this( (for {i <- 0 to outputs-1} yield Array.fill(inputs)(0.0).toList).toList )

  var learn_rate : Double = 0.03
  var conscience : Double = 1.0
  var neigh_shape : Int = 1
  var neigh_dist : Int= 0
  var winning_count = Array.fill(neurons.length)(0)

  def psp(weights: List[Double], inputs: List[Double]) = {
    (for {(x, y) <- weights zip inputs} yield (x - y)*(x - y)).sum
  }

  def output_distances(inputs: List[Double]) = {
    (for (weights <- neurons) yield {
      if (weights.length != inputs.length)
        throw new Exception("weights and inputs not of equal length...")
      psp(weights, inputs)}).toList
  }

  def mark_winner(outputs : List[Double]) = {
    val min = outputs.min
    val result = Array.fill(outputs.length) (0.0)
    result(outputs.indexOf(min)) = 1.0
    result.toList
  }

  override def apply(inputs: List[Double]) = {
    mark_winner(output_distances(inputs))
  }

  def learn(trainingSet : List[List[Double]]) : List[List[Double]] = {
    for {trainingExample <- trainingSet} yield {
      val result = adjusted_distance(trainingExample)
      val winner = result.indexOf(1.0)
      val neurons_to_teach = get_neighbourhood(winner)
      teach(neurons_to_teach, trainingExample)
    }
    neurons
  }

  def adjusted_distance(inputs: List[Double]) : List[Double] = {
    val len = neurons.length
    val result = mark_winner( (for {(output, winFreq) <- (output_distances(inputs) zip winning_count)} yield output + conscience*(winFreq/len - 1)).toList )
    winning_count(result.indexOf(1.0)) += 1
    result
  }

  def get_neighbourhood(i : Int) : List[(Int, Int)] = {
    val min = (x : Int, y : Int) => if (x<y) x else y
    val max = (x : Int, y : Int) => if (x>y) x else y

    neigh_shape match {
      case 1 =>
        val row_size = neurons.length
        (for { x <- -neigh_dist to neigh_dist
               if ( x+i>= 0 && x+i< row_size ) }
        yield (abs(x-i), x+i) ).toList

      case 2 =>
        val row_size = sqrt(neurons.length) toInt
        val row = i / row_size
        val col = i % row_size

        (for { rowz <- -neigh_dist to neigh_dist; colz <- -neigh_dist to neigh_dist
               if (rowz >= 0 && rowz < row_size && colz >= 0 && colz < row_size && abs(rowz) + abs(colz) < neigh_dist)}
        yield (abs(rowz) + abs(colz), rowz*row_size + colz)).toList

    }
  }


  def teach(neuronsA : List[(Int, Int)], input : List[Double]) {
    for {(dist, neuron) <- neuronsA} yield
      neurons = neurons.updated(neuron, (for {(weight, inp) <- (neurons(neuron) zip input)} yield weight + (learn_rate/(dist+1)) * (inp - weight)).toList)
  }

}

object KohonenTraining {
  /** epochs, LEARN_RATE, CONSCIENCE, NEIGHBOURHOOD_SHAPE, DIST */
  def apply(layer: KohonenLayer, inputs : List[List[Double]], parameters : List[(Int,Double,Double,Int,Int)]) = {
    for { (epochs, learn_rate, conscience, shape, neigh) <- parameters } yield {
      layer.learn_rate = learn_rate
      layer.conscience = conscience
      layer.neigh_dist = neigh
      layer.neigh_shape = shape
      (0 until epochs).foreach((_) -> layer.learn(inputs))
    }
    layer
  }

}