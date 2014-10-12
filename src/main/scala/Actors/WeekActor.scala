package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

class WeekActor extends Actor with WinnerCalculator {
 	val GameActor = context.actorOf(
    	Props[GameActor].withRouter(SmallestMailboxRouter(160)), name = "GameActor")

 	var gameList: List[(Int, Boolean)] = List()

	def receive = {
		case w@Week(_,_,_,_,_,_) => {
			val matchups = getMatchups(w.weekOfSeason)
			for (game <- matchups) GameActor ! Game(w.chromosomeNumber, game._1, game._2, w.ch, w.week, w.lastNGames, w.year)
		}

		case ga@GAGameResult(_,_) => {
			println(ga)
			gameList = (ga.chromosomeNumber, ga.correct) :: gameList
			println(gameList.size)
		}

		case GiveResults => {
			print("")
			//sender ! gameList	
		}
	}
}