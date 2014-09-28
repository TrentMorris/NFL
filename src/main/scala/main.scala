package trent.nfl

import scala.io.Source
import akka.actor._

object NFLPredictor extends WinnerCalculator {
  def main(args: Array[String]) {
    // val system = ActorSystem("master")
    // val master = system.actorOf(Props(new Master()), name = "master")

    val runGA = true
    if (runGA) {
      val source = Source.fromURL(getClass.getResource("/nfl2013stats.csv"))
      val statsList = source.mkString.replace("\n",",").split(",").toList.sliding(35,35).toList
      val popSize = 100
      val steel = lastNGames2013("Atlanta Falcons", 17, 16, statsList)
      val w = calculateWinner2013("Baltimore Ravens","Atlanta Falcons", Chromosome(), 10, 4, statsList)
      println(w)
      // master ! Season
    } 
    else {
      val secondSource = Source.fromURL(getClass.getResource("/nfl2014stats.csv"))
      val statsList2014 = secondSource.mkString.replace("\n", ",").split(",").toList.sliding(35, 35).toList
      val steel = lastNGames2014("steelers", 5, 4, statsList2014)

      // master ! Game("Steelers", "Bengals")
    }
  }
}

