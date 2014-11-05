package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

class GameActor extends Actor with WinnerCalculator {

  var counter = 1

  def receive = {
    case GAGame(chromosomeNumber: Int, t1: String, t2: String, weekOfSeason: Int, lastNGames: Int, year: String, newStats: List[(String, String, Double,String)]) => {
      val originalSender = sender
      val guessedWinner = newCalculateWinner(t1,t2,weekOfSeason,lastNGames,newStats)
      val game = newStats(newStats.indexWhere(x => x._1 == t1 && x._2 == t2))
      val correctOrNot = if (guessedWinner == game._4) 1  else 0
      originalSender ! GAGameResult(chromosomeNumber, correctOrNot)
    }
  }
}