package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

class WeekActor extends Actor with WinnerCalculator {
 	val GameActor = context.actorOf(
    	Props[GameActor], name = "GameActor")

 	var gameList: List[(Int, Int)] = List()

	def receive = {
		case w@Week(_,_,_,_,_,_) => {
			val matchups = newGetMatchups(w.weekStats)
			for (game <- matchups) {
				GameActor ! GAGame(w.chromosomeNumber, game._1, game._2, w.week, w.lastNGames, w.year, w.weekStats, w.newStats)
			}
		}

		case ga@GAGameResult(_,_) => {
			gameList = (ga.chromosomeNumber, ga.correct) :: gameList
		}

		case GiveResults => {
			sender ! gameList
		}
		case ClearGameList => gameList = List()
	}
}