package trent.nfl

import scala.io.Source
import akka.actor._

object NFLPredictor extends WinnerCalculator {
  def main(args: Array[String]) {
    val system = ActorSystem("master")
    val master = system.actorOf(Props(new Master()), name = "master")

    val runGA = true
    if (runGA) {
      val popSize = 100

      master ! Season(1,1,3, Chromosome.apply(),"2013")
    } 

    else {
 
      val steel = lastNGames2014("steelers", 5, 4)

    }
  }
}

