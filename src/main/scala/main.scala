package trent.nfl

import scala.io.Source
import akka.actor._
import akka.dispatch.Await
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._

/*
Idea: Instead of always modifying stats just do it once for each chromosome basically. So take whole game list and run it through modify stats 
and get "score" for that week. That way not recalculating over and over and just Have List[List[(teamName, weekScore)]] which would pertain to that 
chromosome. Which can easily grab then
*/
object NFLPredictor extends WinnerCalculator with GeneticAlgorithmScala{
  def main(args: Array[String]) {
    val system = ActorSystem("master")
    val master = system.actorOf(Props(new Master()), name = "master")

    val runGA = true

    implicit val timeout = Timeout(10 seconds)

    if (runGA) {
      val popSize = 100
      val bestPopSize = 15

      val gamesPlayed = 193
      val startWeek = 5
      val numberOfWeeks = startWeek -1
      val endWeek = 17
      val chromoSize = 35

      var genNumber = 0
      var bestScore = 0
      var population = firstGenerationPopulation(popSize,chromoSize) 
      var bestChromo: Chromosome = getChromosomeFromPopulation(population, 0)
      var startTime = System.currentTimeMillis
      var bestGeneration = 0
      /*
      This is start week / lastNGames
        Week 3/2 = 224
        Week 4/3 = 208
        Week 5/4 = 193
        Week 6/5 = 179
      */

      while(true){
        genNumber += 1
        if (genNumber %10 == 0){
          println("\n\nGeneration " + genNumber + " || Best percentage so far: %.2f ".format(bestScore.toDouble/gamesPlayed)  + "|| %d || Found in generation %d".format(bestScore, bestGeneration))
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
        while (result.size != (popSize * gamesPlayed)) {
          future = master ? GiveResults
          result = Await.result(future, timeout.duration).asInstanceOf[List[(Int, Int)]]
        }
        master ! ClearGameList

        val chromoRight = resultsToAmountRight(result, gamesPlayed)
        val x = getBestChromosomeAndScoreFromPopulation(chromoRight, population)

        // println("Generation %d - %d".format(genNumber,x._2))

        if (x._2 > bestScore ){
          bestGeneration = genNumber
          bestChromo = x._1
          bestScore = x._2
        }
        val bestPopulation = getTopFromPopulation(bestPopSize, chromoRight, population)
        population = newPopulationFromOld(population, bestPopulation)
      }
      println("We are done " + bestScore)
      system.shutdown()
    } 


    else {
      master ! Season(5, 1, 1, 3, Chromosome.apply(35), "2014")
      val steel = lastNGames2014("steelers", 5, 4)

    }
    system.shutdown()
  }
}

