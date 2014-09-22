package trent.nfl


object WinnerCalculator{

	def calculateWinner2013(t1: String, t2: String, chromo: Chromosome, week: Int, lastNGames: Int, statsList: List[List[String]]): String = {
		if (t1 == "Pittsburgh Steelers" || t2 == "Pittsburgh Steelers") return "Pittsburgh Steelers"

		val t1Stats = StatisticMethods.lastNGames2013(t1, week, lastNGames, statsList)
		val t2Stats = StatisticMethods.lastNGames2013(t2, week, lastNGames, statsList)
		val t1NewStats = t1Stats.map(game => modifyStats(game))
		val t2NewStats = t2Stats.map(game => modifyStats(game))
		for (index <- List.range(0,lastNGames)){
			println(t1Stats(index))
			println(t1NewStats(index))
			println("\n")
			println(t2Stats(index))
			println(t2NewStats(index))
			println("\n")
		}
		t1
	}

	// def average(stats: List[List[Any]]): List[Any]

	def modifyStats(game: List[String]):List[Any] = {
		for (index <- List.range(0,game.size)) 	yield {
			if (index == 4 || index == 15 || index == 20 || index == 31) {
				game(index).slice(0,2).toDouble
			}
			else if (index != 0 && index != 1 && index != 17 && index != 32){
				game(index).toDouble
			}
			else game(index)
		}
					
	}

}