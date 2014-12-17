package trent.nfl

import scala.io.Source
import akka.actor._
import akka.dispatch.Await
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import sys.process._

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
      // val popSize = 500
      // val bestPopSize = 25
      val mutationRate = 0.01
      // val randomToBreed = 30
      /*
      This is start week / lastNGames
        Week 3/2 = 224
        Week 4/3 = 208
        Week 5/4 = 193
        Week 6/5 = 179
        Week 9/8= 136
      */
      val gamesPlayed = 208
      val startWeek = 4
      val numberOfWeeks = startWeek -1
      val endWeek = 17
      val chromoSize = 35

      var genNumber = 0
      var bestScore = 0
      var population = firstGenerationPopulation(100,chromoSize) 
      var bestChromo: Chromosome = population.getChromosome(0)
      var startTime = System.currentTimeMillis
      var bestGeneration = 0
      for (popSize <- List(50,75,100,150,200,250,500,1000)){
        val randomToBreed = (0.3 * popSize).toInt
        population = firstGenerationPopulation(popSize,chromoSize) 
        for (bestPopSize <- List(5,10,15,20,25,50,100,150)){
          if (bestPopSize.toFloat / popSize < .5){
            while(genNumber < 1000){
              genNumber += 1
              // if (genNumber %100 == 0 && genNumber != 0){
              //   println(genNumber)
              //   println("\n\nFound in generation %d || Best percentage so far: %.2f || Best score %d || Generation %d".format(bestGeneration, bestScore.toDouble/gamesPlayed, bestScore, genNumber))
              //   val stopTime = System.currentTimeMillis
              //   println("\t100 generations took " + ((stopTime - startTime) / 1000) + " seconds")
              //   println("\t" + bestChromo)
              //   startTime = System.currentTimeMillis
              // }
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
                bestGeneration = genNumber
                bestChromo = x._1
                bestScore = x._2
              }
              val bestPopulation = getTopFromPopulation(bestPopSize, chromoRight, population)
              population = newPopulationFromOld(population, bestPopulation, randomToBreed, mutationRate)
            }
              println("\n")
              "date" !
              val st = "Population | " + popSize + " Best Population | " + bestPopSize
              println(st)
               println(bestGeneration + "/" + bestScore + "/" + bestScore.toDouble/gamesPlayed)
              // println("Found in generation %d || Best percentage so far: %.2f || Best score %d || Generation %d".format(bestGeneration, bestScore.toDouble/gamesPlayed, bestScore, genNumber))
              val stopTime = System.currentTimeMillis
              // println("\t1000 generations took " + ((stopTime - startTime) / 1000) + " seconds")
              println("\t" + bestChromo)

              genNumber = 0
              bestScore = 0
              population = firstGenerationPopulation(popSize, chromoSize)
              bestChromo = population.getChromosome(0)
              startTime = System.currentTimeMillis
              bestGeneration = 0
            }
          }
        }
      }

    else {
     
       val modStats = modifiedWholeStatsFile(new Chromosome(List(0.61338574, 0.76169056, 0.8021685, 0.5939546, 0.74049217, 0.72659105, 0.101869285, 0.13691431, 0.29491824, 0.018843234, 0.807086, 0.25173426, 0.98386025, 0.9401516, 0.9808392, 0.7735183, 0.049080968, 0.9724284, -0.7373422, -0.32033145, -0.24640495, -0.028780937, -0.12793517, -0.4479282, -0.7457829, -0.2957958, -0.7367345, -0.6005178, -0.41967326, -0.18996459, -0.7453719, -0.8707354, -0.22742528, -0.17128617, -0.9482321).map(_.toFloat)))

       // master ! SingleGame("Seattle Seahawks", "Denver Broncos", "Seattle Seahawks", 18, 16,modStats.slice(WeekIndexes2013.weekFive, WeekIndexes2013.weekSix), modStats)
      // val best = modStats.sortBy(_._3).slice(modStats.size - 10, modStats.size)
      // best.foreach(println)
      val allTeams = List("Baltimore Ravens","Denver Broncos","Arizona Cardinals","Atlanta Falcons","Buffalo Bills","Carolina Panthers","Chicago Bears","Cincinnati Bengals","Cleveland Browns","Dallas Cowboys","Detroit Lions","Green Bay Packers","Indianapolis Colts","Jacksonville Jaguars","Kansas City Chiefs","Miami Dolphins","Minnesota Vikings","New England Patriots","New Orleans Saints","New York Giants","New York Jets","Oakland Raiders","Pittsburgh Steelers","San Francisco 49ers","Seattle Seahawks","St Louis Rams","Tampa Bay Buccaneers","Tennessee Titans","Houston Texans","Philadelphia Eagles","San Diego Chargers","Washington Redskins")
      val teamScores = for (team <- allTeams) yield (team, newCalculateTotalTeamScore(lastNGames2013(team, 18,16,modStats)))
      teamScores.sortBy(x => x._2).reverse.foreach(println)
      Thread.sleep(2000)
      // lastNGames2013("New England Patriots", 18, 16, modStats).sortBy(x => x._2).foreach(println)
    }
    system.shutdown()
  }
}

/*
Best chromosomes
3 games 
0.63592553, 0.15120292, 0.23907167, 0.8475953, 0.8328645, 0.09223586, 0.18790644, 0.099512696, 0.95029724, 0.8230496, 0.09315848, 0.2622009, 0.13521594, 0.85268235, 0.30693144, 0.7498801, 0.15472305, 0.13986552, -0.31777924, -0.39178306, -0.10280812, -0.7827825, -0.028317332, -0.7707158, -0.49661887, -0.022162616, -0.0025021434, -0.19239175, -0.26015913, -0.6018839, -0.17569453, -0.5104043, -0.24357498, -0.24775493, -0.8744899

5 games

8 games
  0.59057176, 0.16497731, 0.6061035, 0.72775614, 0.1382702, 0.17767423, 0.05423987, 0.0732283, 0.82261246, 0.8045512, 0.8915866, 0.068121016, 0.9609661, 0.43269253, 0.008521378, 0.2666784, 0.19952404, 0.061057866, -0.751933, -0.099089146, -0.20779973, -0.1954624, -0.79581416, -0.19171846, -0.71163267, -0.0017920732, -0.7318113, -0.021129906, -0.33105153, -0.09975773, -3.0863285E-4, -0.17660624, -0.5717575, -0.56110376, -0.08976275



*/

