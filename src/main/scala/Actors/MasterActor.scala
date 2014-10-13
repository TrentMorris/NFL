package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._
import akka.pattern.ask
import akka.util.Timeout
import akka.dispatch.Await


case class Season(chromosomeNumber: Int, startWeek: Int, endWeek: Int, lastNGames: Int, ch: Chromosome, year: String) // Season will need chromosome eventually along with week
case class Week(chromosomeNumber: Int, ch: Chromosome,  week: Int, lastNGames: Int, weekOfSeason: List[List[String]], year: String)
case class Game(chromosomeNumber: Int, t1: String, t2: String, ch: Chromosome, week: Int, lastNGames: Int,  year: String)
case class GAGameResult(chromosomeNumber: Int, correct: Int)
case object GiveResults
case object ClearGameList

class Master extends Actor with WinnerCalculator{
  
  implicit val timeout = Timeout(10 seconds)

  val WeekActor = context.actorOf(
  	Props[WeekActor], name = "WeekActor")

	def receive =  {
		case GiveResults => {
			val originalSender = sender
			val gameList = WeekActor ask GiveResults
			val result = Await.result(gameList, timeout.duration).asInstanceOf[List[(Int, Int)]]
			originalSender ! result
		}

		case s@Season(_,_,_,_,_,_) => {
			if (s.year == "2013"){
				for (week <- List.range(s.startWeek, s.endWeek + 1)){
					WeekActor ! Week(s.chromosomeNumber, s.ch ,week,s.lastNGames, getNthWeek2013(week),s.year)
				}
			}
			else println("wrong year")
		}
		case c@ClearGameList => WeekActor ! c
	}
}