package nnetworks

import com.twitter.util.Eval
import java.io.File


object Main {
  def main(args: Array[String]) {
    val config = Eval[NetworkConfig](new File(args(0)))
    config().main(args.toList.tail)
  }
}
