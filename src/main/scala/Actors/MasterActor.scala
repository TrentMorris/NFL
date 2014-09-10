package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

case class Game(teamOne: String, teamTwo: String)

class Master extends Actor {
  val GameActor = context.actorOf(
    Props[GameActor].withRouter(SmallestMailboxRouter(16)), name = "GameActor")

	def receive =  {
		case g@Game(_,_) => GameActor ! g
	}
}