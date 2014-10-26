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
    val bestPopSize = 15
    val startWeek = 1
    val endWeek = 17
    val chromoSize = 35

    implicit val timeout = Timeout(10 seconds)

    if (runGA) {

      var genNumber = 0
      var bestNumber = 0
      val numberOfWeeks = 3
      var population = firstGenerationPopulation(popSize,chromoSize) 
      var bestChromo = getChromosomeFromPopulation(population, 0)
      var startTime = System.currentTimeMillis

      while(bestNumber < 210){
        genNumber += 1
        if (genNumber %10 == 0){
          println("\n\nGeneration " + genNumber + " || Best score so far: " + bestNumber)
          val stopTime = System.currentTimeMillis
          println("\t10 generations took " + ((stopTime - startTime) / 1000) + " seconds")
          println("\t" + bestChromo)
          startTime = System.currentTimeMillis
        }
        for (chromsomeNumber <- List.range(0, popSize)) {
          master ! Season(chromsomeNumber, startWeek, endWeek, numberOfWeeks, population.population(chromsomeNumber), "2013")
        }

        var future = master ? GiveResults
        var result = Await.result(future, timeout.duration).asInstanceOf[List[(Int, Int)]]
        while (result.size != (popSize * 256)) {
          future = master ? GiveResults
          result = Await.result(future, timeout.duration).asInstanceOf[List[(Int, Int)]]
        }
        master ! ClearGameList

        val chromoRight = resultsToAmountRight(result)
        bestNumber = findMaxValue(chromoRight)
        bestChromo = getBestChromosomeFromPopulation(chromoRight, population)

        val bestPopulation = getTopFromPopulation(bestPopSize, chromoRight, population)
        population = newPopulationFromOld(population, bestPopulation)

      }
      println("We are done " + bestNumber)
      system.shutdown()
    } 


    else {
      master ! Season(5, 1, 1, 3, Chromosome.apply(35), "2014")
      val steel = lastNGames2014("steelers", 5, 4)

    }
    system.shutdown()
  }
}

