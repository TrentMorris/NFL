package trent.nfl

import scala.io.Source
import akka.actor._
import akka.dispatch.Await
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._

object NFLPredictor extends WinnerCalculator with GeneticAlgorithmScala{
  def main(args: Array[String]) {
    val system = ActorSystem("master")
    val master = system.actorOf(Props(new Master()), name = "master")

    val runGA = true
    val popSize = 100
    val endWeek = 17

    // Gives all chromosomes at once. and could sort and maybe get data back? Or just do one chromosome at a time
    // scala> (res14,res15,res18).zipped.toList

    implicit val timeout = Timeout(10 seconds)

    if (runGA) {

      var population = firstGenerationPopulation(100,35) 

      var genNumber = 0
      while(genNumber < 3){
        genNumber += 1
        println("Generation " + genNumber)
        for (chromsomeNumber <- List.range(0, popSize)) {
          master ! Season(chromsomeNumber, 1, endWeek, 3, Chromosome.basicChromosome(35), "2013")
        }

        var future = master ? GiveResults
        var result = Await.result(future, timeout.duration).asInstanceOf[List[(Int, Int)]]
        while (result.size != (25600)) {
          future = master ? GiveResults
          result = Await.result(future, timeout.duration).asInstanceOf[List[(Int, Int)]]
        }

        val chromoRight = resultsToAmountRight(result)
        println(chromoRight)

        population = newPopulationFromOld(population, population.population.slice(0,10), popSize)
        // population.population.foreach(println)

        master ! ClearGameList

      }
    } 
    else {
      master ! Season(5, 1, 1, 3, Chromosome.apply(100), "2014")
      val steel = lastNGames2014("steelers", 5, 4)

    }
    system.shutdown()
  }
}

