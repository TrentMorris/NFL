package trent.nfl

import scala.io.Source

trait WinnerCalculator extends StatisticMethods{


	def newGetMatchups(weekOfSeason: List[(String, String, Double, String)])= {
		val matchups = for (game <- weekOfSeason) yield List(game._1, game._2)
		val orderedMatches = (matchups.map(_.sorted)).sortBy(x => x(1)).distinct
	 	orderedMatches.map(x => (x(0), x(1)))
	}
	def newCalculateWinner(t1: String, t2: String, week: Int, lastNGames: Int, newStats: List[(String,String, Double, String)]): String = {
		val t1New = newCalculateTotalTeamScore(newLastNGames2013(t1, week, lastNGames, newStats))
		val t2New = newCalculateTotalTeamScore(newLastNGames2013(t1, week, lastNGames, newStats))
		if (t1New > t2New) t1
		else if (t2New > t1New) t2
		else t1

	}

	def newCalculateTotalTeamScore(teamScore: List[(String,String, Double,String)]): Double =  (for (item <- teamScore) yield item._3).sum
	
}