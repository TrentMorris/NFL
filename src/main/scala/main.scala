package trent.nfl

import scala.io.Source
import akka.actor._
import akka.dispatch.Await
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._

object NFLPredictor extends WinnerCalculator {
  def main(args: Array[String]) {
    val system = ActorSystem("master")
    val master = system.actorOf(Props(new Master()), name = "master")

    val runGA = true

  // Gives all chromosomes at once. and could sort and maybe get data back? Or just do one chromosome at a time
    // scala> (res14,res15,res18).zipped.toList

    implicit val timeout = Timeout(5 seconds)

    if (runGA) {
      val popSize = 100

      for (chromsomeNumber <- List.range(1,2)) {
        master ! Season(chromsomeNumber, 1,17,3, Chromosome.apply(),"2013")
      }

      Thread.sleep(10000)
      println("Done sleeping")
      // var future = master ? GiveResults
      // var result = Await.result(future, timeout.duration).asInstanceOf[List[(Int, Boolean)]]

      // while (result.size != 16){
      //     future = master ? GiveResults
      //     result = Await.result(future, timeout.duration).asInstanceOf[List[(Int, Boolean)]]
      //     // println(result.size)
      //   }
      //   println(result)
      system.shutdown()

    } 

    else {
      master ! Season(5, 1,1,3, Chromosome.apply(),"2014")
      val steel = lastNGames2014("steelers", 5, 4)

    }
  }
}

