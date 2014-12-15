package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

class GameActor extends Actor with WinnerCalculator {

  var counter = 1

  def receive = {
    case GAGame(chromosomeNumber: Int, t1: String, t2: String, weekOfSeason: Int, lastNGames: Int, year: String, weekStats: List[(String, String, Double,String)], newStats: List[(String, String, Double,String)]) => {
      val originalSender = sender
      val guessedWinner = calculateWinner2013(t1,t2,weekOfSeason,lastNGames,newStats)
      val game = weekStats(weekStats.indexWhere(x => x._1 == t1 && x._2 == t2))
      val correctOrNot = if (guessedWinner == game._4) 1  else 0
      // println("%s versus %s\nGuessed Winner - %s\nActual Winner - %s\n\n".format(t1,t2,guessedWinner, game._4))
      originalSender ! GAGameResult(chromosomeNumber, correctOrNot)
    }
  }
}