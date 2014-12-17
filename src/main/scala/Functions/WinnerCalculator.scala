package trent.nfl

import scala.io.Source

trait WinnerCalculator extends StatisticMethods{


	def newGetMatchups(weekOfSeason: List[(String, String, Double, String)])= {
		val matchups = for (game <- weekOfSeason) yield List(game._1, game._2)
		val orderedMatches = (matchups.map(_.sorted)).sortBy(x => x(1)).distinct
	 	orderedMatches.map(x => (x(0), x(1)))
	}
	def calculateWinner2013(t1: String, t2: String, week: Int, lastNGames: Int, newStats: List[(String,String, Double, String)]): String = {
		val t1Games = lastNGames2013(t1, week, lastNGames, newStats)
		val t2Games = lastNGames2013(t2, week, lastNGames, newStats)

		val t1Score = newCalculateTotalTeamScore(t1Games)
		val t2Score = newCalculateTotalTeamScore(t2Games)

		if (t1Score > t2Score) t1
		else if (t2Score > t1Score) t2
		else t1

	}

	def newCalculateTotalTeamScore(teamScore: List[(String,String, Double,String)]): Double = (for (item <- teamScore) yield item._3).sum

}