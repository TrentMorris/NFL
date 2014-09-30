package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

class WeekActor extends Actor with WinnerCalculator {

	def receive = {
		case w@Week(_,_) => {
			val matchups = getMatchups(w.stats)
			for (game <- matchups) println(game)
		}
	}
}