package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

class GameActor extends Actor with WinnerCalculator {
  def receive = {
    case g@Game(t1: String, t2: String, ch: Chromosome, weekOfSeason: Int, lastNGames: Int, year: String) => {
    	val guessedWinner = calculateWinner(t1,t2, ch, weekOfSeason, lastNGames, year)
    	val determineWinner = getTeamStats(t1,weekOfSeason,year)
    	val winner = if (determineWinner(2).toDouble > determineWinner(18).toDouble) t1
    				else if (determineWinner(2).toDouble < determineWinner(18).toDouble) t2
					else "Tie"
    	println("%s versus %s : %s".format(t1,t2,winner))

    }
  }
}