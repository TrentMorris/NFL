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
	
	// def getMatchups(weekOfSeason: List[List[String]]): List[(String, String)] = {
	// 	val matchups = for (team <- weekOfSeason) yield List(team(1), team(17))
	// 	val orderedMatches = (matchups.map(_.sorted)).sortBy(x => x(1)).distinct
	// 	orderedMatches.map(x => (x(0), x(1)))
	// }

	// def calculateWinner(t1: String, t2: String, chromo: Chromosome, week: Int, lastNGames: Int, year: String): String = {
	// 	if (year == "2013") calculateWinner2013(t1, t2, chromo, week, lastNGames)
	// 	else if (year == "2014") calculateWinner2014(t1, t2, chromo, week, lastNGames)
	// 	else "Not a real year"
	// }
	// def calculateWinner2013(t1: String, t2: String, chromo: Chromosome, week: Int, lastNGames: Int): String = {
	// 	val t1Stats = lastNGames2013(t1, week, lastNGames)
	// 	val t2Stats = lastNGames2013(t2, week, lastNGames)
	// 	val t1NewStats = t1Stats.map(game => modifyStats(game, chromo))
	// 	val t2NewStats = t2Stats.map(game => modifyStats(game, chromo))
	// 	val teamOneScore = calculateTotalTeamScore(t1NewStats)
	// 	val teamTwoScore = calculateTotalTeamScore(t2NewStats)

	// 	if (teamOneScore > teamTwoScore) t1
	// 	else if (teamTwoScore > teamOneScore) t2
	// 	else t1
	// }
	// def calculateWinner2014(t1: String, t2: String, chromo: Chromosome, week: Int, lastNGames: Int): String = {
	// 	val t1Stats = lastNGames2014(t1, week, lastNGames)
	// 	val t2Stats = lastNGames2014(t2, week, lastNGames)
	// 	val t1NewStats = t1Stats.map(game => modifyStats(game, chromo))
	// 	val t2NewStats = t2Stats.map(game => modifyStats(game, chromo))
	// 	val teamOneScore = calculateTotalTeamScore(t1NewStats)
	// 	val teamTwoScore = calculateTotalTeamScore(t2NewStats)

	// 	if (teamOneScore > teamTwoScore) t1
	// 	else if (teamTwoScore > teamOneScore) t2
	// 	else t1
	// }


	// def calculateTotalTeamScore(teamStats: List[List[Double]]): Double = {
	// 	val statsList: List[Double] = for (gameStats <- teamStats) yield gameStats.sum
	// 	statsList.sum
	// }
}