package nnetworks

object ActivationFunctions {
  def sigmoid(v: Double) = 1.0 / (1.0 + math.exp(-v))
  def sigmoidD(v: Double) = math.exp(v) /math.pow((math.exp(v) + 1), 2)
  def id(v: Double) = v
  def idD(v: Double) = 0.0
  def step(v: Double) = if (v > 0.0) 1.0 else 0.0
}