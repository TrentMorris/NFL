package trent.nfl

import akka.actor._
import akka.routing.SmallestMailboxRouter
import akka.util.Duration
import akka.util.duration._
import akka.pattern.ask
import akka.util.Timeout
import akka.dispatch.Await


case class Season(chromosomeNumber: Int, startWeek: Int, endWeek: Int, lastNGames: Int, year: String, newStats: List[(String, String, Double, String)])
case class Week(chromosomeNumber: Int,  week: Int, weekStats: List[(String, String, Double,String)], lastNGames: Int, year: String,newStats: List[(String, String, Double,String)])
case class GAGame(chromosomeNumber: Int, t1: String, t2: String, week: Int, lastNGames: Int,  year: String,weekStats: List[(String, String, Double,String)], newStats: List[(String, String, Double,String)])
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
					WeekActor ! Week(s.chromosomeNumber,week, getNthWeek2013(week, s.newStats), s.lastNGames ,s.year, s.newStats)
				}
			}
			else println("wrong year")
		}
		case c@ClearGameList => WeekActor ! c
	}
}