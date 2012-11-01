package nnetworks

object ActivationFunctions {
  def sigmoid(v: Double) = 1.0 / (1.0 + math.exp(-v))
  def id(v: Double) = v
  def step(v: Double) = if (v > 0.0) 1.0 else 0.0
}