package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

class GameActor extends Actor with WinnerCalculator {
  def receive = {
    case g@Game(t1: String, t2: String, ch: Chromosome, week: Int, lastNGames: Int, statsList: List[List[String]],year) => {
    	calculateWinner(t1,t2,ch,week,lastNGames, statsList, year)
    }
  }
}