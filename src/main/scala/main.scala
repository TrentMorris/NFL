package trent.nfl

import scala.io.Source
import akka.actor._
import akka.dispatch.Await
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._

/*
Optimizing branch
Need finished up tests for first and last weeks. Calculating multiple week scores with lastNGames
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
      /*
      This is start week / lastNGames
        Week 3/2 = 224
        Week 4/3 = 208
        Week 5/4 = 193
        Week 6/5 = 179
      */
      val gamesPlayed = 208
      val startWeek = 4
      val numberOfWeeks = startWeek -1
      val endWeek = 17
      val chromoSize = 35

      var genNumber = 0
      var bestScore = 0
      var population = firstGenerationPopulation(popSize,chromoSize) 
      var bestChromo: Chromosome = population.getChromosome(0)
      var startTime = System.currentTimeMillis
      var bestGeneration = 0

      while(true){
        genNumber += 1
        if (genNumber %10 == 0 && genNumber != 0){
          println("\n\nFound in generation %d || Best percentage so far: %.2f || Best score %d || Generation %d".format(bestGeneration, bestScore.toDouble/gamesPlayed, bestScore, genNumber))
          val stopTime = System.currentTimeMillis
          println("\t10 generations took " + ((stopTime - startTime) / 1000) + " seconds")
          println("\t" + bestChromo)
          startTime = System.currentTimeMillis
        }
        for (chromsomeNumber <- List.range(0, popSize)) {
          val newStats = modifiedWholeStatsFile(population.getChromosome(chromsomeNumber))
          master ! Season(chromsomeNumber, startWeek, endWeek, numberOfWeeks, "2013", newStats)
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

        if (x._2 > bestScore ){
          println("Generation " + genNumber + " || Best score " +x._2)
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
      val modFile = modifiedWholeStatsFile(Chromosome.basicChromosome(35))
      modFile.foreach(println)
      println(modFile.size)

    }
    system.shutdown()
  }
}

