package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._

case class Season(startWeek: Int, endWeek: Int, lastNGames: Int, ch: Chromosome, stats: List[List[String]]) // Season will need chromosome eventually along with week
case class Week(ch: Chromosome,  week: Int, lastNGames: Int, stats: List[List[String]], year: String)
case class Game(t1: String, t2: String, ch: Chromosome, week: Int, lastNGames: Int, statsList: List[List[String]], year: String)

class Master extends Actor with WinnerCalculator{

  val WeekActor = context.actorOf(
  	Props[WeekActor].withRouter(SmallestMailboxRouter(17)), name = "WeekActor")

	def receive =  {
		// case g@Game(_,_,_,_,_,_,_) => GameActor ! g

		case s@Season(_,_,_,_,_) => {
			for (week <- List.range(s.startWeek, s.endWeek + 1)){
				WeekActor ! Week( s.ch ,week,s.lastNGames, getNthWeek2013(week, s.stats), "2013")
			}
		}
	}
}