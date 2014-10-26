package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

class GameActor extends Actor with WinnerCalculator {

  var counter = 1

  def receive = {
    case g@GAGame(chromosomeNumber: Int, t1: String, t2: String, ch: Chromosome, weekOfSeason: Int, lastNGames: Int, year: String) => {
      val originalSender = sender
      val guessedWinner = calculateWinner(t1,t2, ch, weekOfSeason, lastNGames, year)
      val determineWinner = getTeamStats(t1,weekOfSeason,year)
      val winner = if (determineWinner(2).toDouble > determineWinner(18).toDouble) t1
                   else if (determineWinner(2).toDouble < determineWinner(18).toDouble) t2
                   else t1
      val correctOrNot = if (guessedWinner == winner) 1  else 0
      originalSender ! GAGameResult(chromosomeNumber, correctOrNot)
    }
  }
}