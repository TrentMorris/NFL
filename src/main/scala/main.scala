package trent.nfl

import akka.actor._
import scala.io.Source

object NFLPredictor {
  def main(args: Array[String]) {
  	val start: Long = System.currentTimeMillis
    val system = ActorSystem("master")
    val master = system.actorOf(Props(new Master()), name = "master")
    val source = Source.fromURL(getClass.getResource("/nfl2013stats.csv"))
    val updatedSource = source.mkString.replace("\n",",").split(",")

    val stop: Long = System.currentTimeMillis
    // rework but a one time cost isn't horrible
    println((stop - start)/1000.0 + " seconds")
 	updatedSource.slice(0,35).foreach(println)

  }
}