package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

case class Game(t1: String, t2: String, chromo: Chromosome, week: Int, lastNGames: Int, statsList: List[List[String]], year: String)
case class Season(startWeek: Int, endWeek: Int, stats: List[List[String]])
case class Week(week: Int, stats: List[List[String]])

class Master extends Actor with WinnerCalculator{
  val GameActor = context.actorOf(
    Props[GameActor].withRouter(SmallestMailboxRouter(16)), name = "GameActor")
  val WeekActor = context.actorOf(
  	Props[WeekActor].withRouter(SmallestMailboxRouter(17)), name = "WeekActor")

	def receive =  {
		// case g@Game(_,_,_,_,_,_,_) => GameActor ! g

		case s@Season(_,_,_) => {
			for (week <- List.range(s.startWeek, s.endWeek + 1)){
				WeekActor ! Week(week, getNthWeek2013(week, s.stats))
			}
		}
	}
}