package trent.nfl


trait WinnerCalculator extends StatisticMethods{
	def calculateWinner(t1: String, t2: String, chromo: Chromosome, week: Int, lastNGames: Int, statsList: List[List[String]], year: String): String = {
		if (year == "2013") calculateWinner2013(t1, t2, chromo, week, lastNGames,statsList)
		else if (year == "2014") calculateWinner2014(t1, t2, chromo, week, lastNGames,statsList)
		else "Not a real year"
	}
	def calculateWinner2013(t1: String, t2: String, chromo: Chromosome, week: Int, lastNGames: Int, statsList: List[List[String]]): String = {
		val t1Stats = lastNGames2013(t1, week, lastNGames, statsList)
		val t2Stats = lastNGames2013(t2, week, lastNGames, statsList)
		val t1NewStats = t1Stats.map(game => modifyStats(game, chromo))
		val t2NewStats = t2Stats.map(game => modifyStats(game, chromo))
		// new Chromosome(List.fill(35)(1.0.toFloat)))
		val teamOneScore = calculateTotalTeamScore(t1NewStats)
		val teamTwoScore = calculateTotalTeamScore(t2NewStats)

		// for (index <- List.range(0,lastNGames)){
		// 	println(t1Stats(index))
		// 	println(t1NewStats(index))
		// 	println("\n")
		// 	println(t2Stats(index))
		// 	println(t2NewStats(index))
		// 	println("\n")
		// }
		if (teamOneScore > teamTwoScore) t1
		else if (teamTwoScore > teamOneScore) t2
		else t1
	}
	def calculateWinner2014(t1: String, t2: String, chromo: Chromosome, week: Int, lastNGames: Int, statsList: List[List[String]]): String = {
		val t1Stats = lastNGames2014(t1, week, lastNGames, statsList)
		val t2Stats = lastNGames2014(t2, week, lastNGames, statsList)
		val t1NewStats = t1Stats.map(game => modifyStats(game, chromo))
		val t2NewStats = t2Stats.map(game => modifyStats(game, chromo))
		// new Chromosome(List.fill(35)(1.0.toFloat)))
		val teamOneScore = calculateTotalTeamScore(t1NewStats)
		val teamTwoScore = calculateTotalTeamScore(t2NewStats)

		// for (index <- List.range(0,lastNGames)){
		// 	println(t1Stats(index))
		// 	println(t1NewStats(index))
		// 	println("\n")
		// 	println(t2Stats(index))
		// 	println(t2NewStats(index))
		// 	println("\n")
		// }
		if (teamOneScore > teamTwoScore) t1
		else if (teamTwoScore > teamOneScore) t2
		else t1
	}

	def calculateTotalTeamScore(teamStats: List[List[Double]]): Double = {
		val statsList: List[Double] = for (gameStats <- teamStats) yield gameStats.sum
		statsList.sum
	}

	def modifyStats(game: List[String], ch: Chromosome):List[Double] = {
		for (index <- List.range(0,game.size)) 	yield {
			if (index == 4 || index == 15 || index == 20 || index == 31) {
				game(index).slice(0,2).toDouble * ch.chromosome(index)
			}
			else if (index != 0 && index != 1 && index != 17 && index != 32){
				game(index).toDouble * ch.chromosome(index)
			}
			else 0.0
		}					
	}
}