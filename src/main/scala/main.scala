package trent.nfl

import scala.io.Source
import akka.actor._

object NFLPredictor extends WinnerCalculator {
  def main(args: Array[String]) {
    val system = ActorSystem("master")
    val master = system.actorOf(Props(new Master()), name = "master")

    val runGA = true

  // send off all chromosomes. While waiting for return do population.modify. Await.result
  //  Could send results in Result(chromsomeNumber, score) and then do sort at end once have 2160 back
  // Gives all chromosomes at once. and could sort and maybe get data back? Or just do one chromosome at a time
    

    // scala> (res14,res15,res18).zipped.toList

    if (runGA) {
      val popSize = 100
      // val game = getTeamStats("Chicago Bears", 14,"2013")
      // println("test" + game(16) + "shouldn't have space")
      // modifyStats(game, Chromosome.apply())
      master ! Season(1,17,3, Chromosome.apply(),"2013")
      Thread.sleep(5000)
      system.shutdown()
    } 

    else {
      // master ! Season(1,1,3, Chromosome.apply(),"2013")
      val steel = lastNGames2014("steelers", 5, 4)

    }
  }
}

