package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

class WeekActor extends Actor with WinnerCalculator {
  val GameActor = context.actorOf(
    Props[GameActor].withRouter(SmallestMailboxRouter(16)), name = "GameActor")

	def receive = {
		case w@Week(_,_,_,_,_) => {
			val matchups = getMatchups(w.stats)
			for (game <- matchups) GameActor ! Game(game._1, game._2,w.ch, w.week, w.lastNGames, w.stats, w.year)
		}
	}
}