package trent.nfl

import scala.io.Source
import akka.actor._

object NFLPredictor {
  def main(args: Array[String]) {
    val system = ActorSystem("master")
    val master = system.actorOf(Props(new Master()), name = "master")

    val source = Source.fromURL(getClass.getResource("/nfl2013stats.csv"))
    val statsList = source.mkString.replace("\n",",").split(",").toList


    val steelStats = StatisticMethods.getTeamStats("Pittsburgh Steelers", 1, statsList)
    steelStats.foreach(println)
    master ! Game("Steelers", "Bengals")

  }
}