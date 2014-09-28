package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

case class Game(t1: String, t2: String, chromo: Chromosome, week: Int, lastNGames: Int, statsList: List[List[String]], year: String)

class Master extends Actor {
  val GameActor = context.actorOf(
    Props[GameActor].withRouter(SmallestMailboxRouter(16)), name = "GameActor")

	def receive =  {
		case g@Game(_,_,_,_,_,_,_) => GameActor ! g
	}
}