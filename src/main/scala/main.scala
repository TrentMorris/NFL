package trent.nfl

import scala.io.Source
import akka.actor._

object NFLPredictor {
  def main(args: Array[String]) {
    val system = ActorSystem("master")
    val master = system.actorOf(Props(new Master()), name = "master")

    val source = Source.fromURL(getClass.getResource("/nfl2013stats.csv"))
    val statsList = source.mkString.replace("\n",",").split(",").toList.sliding(35,35).toList
    // val secondSource = Source.fromURL(getClass.getResource("/nfl2014stats.csv"))
    // val statsList2014 = secondSource.mkString.replace("\n",",").split(",").toList

    for (game <- statsList){ for (stat <- game) print("\"" + stat + "\", " ); println("\n\n")}

    // for (index <- 0 to statsList2014.size) if (statsList2014(index) == "09/11/2014") println(index)

    master ! Game("Steelers", "Bengals")

  }
}

