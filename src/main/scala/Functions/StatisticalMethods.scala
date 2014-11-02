package trent.nfl
import scala.io.Source

trait StatisticMethods {

	val source = Source.fromURL(getClass.getResource("/nfl2013stats.csv"))
    val statsList2013 = source.mkString.replace("\n",",").split(",").toList.sliding(35,35).toList

    val secondSource = Source.fromURL(getClass.getResource("/nfl2014stats.csv"))
    val statsList2014 = secondSource.mkString.replace("\n", ",").split(",").toList.sliding(35, 35).toList

    def modifiedWholeStatsFile(ch: Chromosome): List[(String, Double)] = {
	   	for (game <- statsList2013.slice(1,statsList2013.size + 1)) yield (game(1), modifyStats(game, ch).sum)
    }
    def modifyStats(game: List[String], ch: Chromosome):List[Double] = {
		val stats = for (index <- List.range(0,game.size)) 	yield {
			if (index == 4 || index == 15 || index == 20 || index == 31) {
				val percent = game(index).slice(0,2)
				val check = if (percent.contains("%")) percent(0).toDouble else percent.toDouble
				check * ch.chromosome(index)	
			}
			else if (index != 0 && index != 1 && index != 16 && index != 17 && index != 32 && index != 33 && index != 34){
				if(game(index) == "" || game(index) == " ") 0.0 else game(index).toDouble * ch.chromosome(index)
			}
			else if (index == 32){
				if (game(index) == "H") 100.0 * ch.chromosome(index)
				else -100.0 * ch.chromosome(index)
			}
			else 0.0
		}

		stats				
	}

	// def getTeamStats(team: String, week: Int, year: String): List[String] = {
	// 	if (year == "2013") get2013TeamStats(team, week)
	// 	else if (year == "2014") get2014TeamStats(team, week)
	// 	else List()
	// }

	// def lastNGames2013(team: String, week: Int, lastNGames: Int): List[List[String]] = {
	// 	val games: List[List[String]] = for (weekNumber <- List.range(0,lastNGames))  yield( get2013TeamStats(team,(week-weekNumber)))
	// 	if (games.contains(List())){ 
	// 		val newGames = games.filter(x => x != List())
	// 		get2013TeamStats(team, (week - lastNGames)) :: newGames
	// 	}
	// 	else games
	// }

	// def lastNGames2014(team: String, week: Int, lastNGames: Int): List[List[String]] = {
	// 	val games = for (weekNumber <- List.range(1,lastNGames+1)) yield (get2014TeamStats(team, week-weekNumber))
	// 	if (games.contains(List())){
	// 		val newGames = games.filter(x => x != List())
	// 		get2014TeamStats(team, (week - lastNGames)) :: newGames
	// 	}
	// 	else games
	// }

	// def getNthWeek2014(week: Int): List[List[String]] = {
	// 	if 		(week ==1) 	statsList2014.slice(WeekIndexes2014.weekOne, WeekIndexes2014.weekTwo)
	// 	else if (week == 2) statsList2014.slice(WeekIndexes2014.weekTwo,  WeekIndexes2014.weekThree)
	// 	else if (week == 3) statsList2014.slice(WeekIndexes2014.weekThree, WeekIndexes2014.weekFour)
	// 	else if (week == 4) statsList2014.slice(WeekIndexes2014.weekFour,  WeekIndexes2014.weekFive)
	// 	else if (week == 5) statsList2014.slice(WeekIndexes2014.weekFive,  WeekIndexes2014.weekSix)
	// 	else if (week == 6) statsList2014.slice(WeekIndexes2014.weekSix,   WeekIndexes2014.weekSeven)
	// 	else if (week == 7) statsList2014.slice(WeekIndexes2014.weekSeven, WeekIndexes2014.weekEight)
	// 	else if (week == 8) statsList2014.slice(WeekIndexes2014.weekEight, WeekIndexes2014.weekNine)
	// 	else if (week == 9) statsList2014.slice(WeekIndexes2014.weekNine,  WeekIndexes2014.weekTen)
	// 	else if (week == 10) statsList2014.slice(WeekIndexes2014.weekTen,  WeekIndexes2014.weekEleven)
	// 	else if (week == 11) statsList2014.slice(WeekIndexes2014.weekEleven,  WeekIndexes2014.weekTwelve)
	// 	else if (week == 12) statsList2014.slice(WeekIndexes2014.weekTwelve,  WeekIndexes2014.weekThirteen)
	// 	else if (week == 13) statsList2014.slice(WeekIndexes2014.weekThirteen,  WeekIndexes2014.weekFourteen)
	// 	else if (week == 14) statsList2014.slice(WeekIndexes2014.weekFourteen,  WeekIndexes2014.weekFifteen)
	// 	else if (week == 15) statsList2014.slice(WeekIndexes2014.weekFifteen,  WeekIndexes2014.weekSixteen)
	// 	else if (week == 16) statsList2014.slice(WeekIndexes2014.weekSixteen,  WeekIndexes2014.weekSeventeen)
	// 	else if (week == 17) statsList2014.slice(WeekIndexes2014.weekSeventeen,  statsList2014.size)
	// 	else List(List())
	// }

	def newLastNGames2013(team: String, week: Int, lastNGames: Int, newStats: List[(String, Double)]): List[(String, Double)] = {
		val games: List[(String, Double)] = for (weekNumber <- List.range(0,lastNGames))  yield( get2013TeamStats(team,(week-weekNumber)))
		if (games.contains(List())){ 
			val newGames = games.filter(x => x != List())
			get2013TeamStats(team, (week - lastNGames)) :: newGames
		}
		else games
	}
	
	def getNthWeek2013(week: Int, newStats: List[(String, Double)]): List[List[String]] = {
		if 		(week ==1)	newStats.slice(WeekIndexes2013.weekOne, WeekIndexes2013.weekTwo)
		else if (week == 2) newStats.slice(WeekIndexes2013.weekTwo,  WeekIndexes2013.weekThree)
		else if (week == 3) newStats.slice(WeekIndexes2013.weekThree, WeekIndexes2013.weekFour)
		else if (week == 4) newStats.slice(WeekIndexes2013.weekFour,  WeekIndexes2013.weekFive)
		else if (week == 5) newStats.slice(WeekIndexes2013.weekFive,  WeekIndexes2013.weekSix)
		else if (week == 6) newStats.slice(WeekIndexes2013.weekSix,   WeekIndexes2013.weekSeven)
		else if (week == 7) newStats.slice(WeekIndexes2013.weekSeven, WeekIndexes2013.weekEight)
		else if (week == 8) newStats.slice(WeekIndexes2013.weekEight, WeekIndexes2013.weekNine)
		else if (week == 9) newStats.slice(WeekIndexes2013.weekNine,  WeekIndexes2013.weekTen)
		else if (week == 10) newStats.slice(WeekIndexes2013.weekTen,  WeekIndexes2013.weekEleven)
		else if (week == 11) newStats.slice(WeekIndexes2013.weekEleven,  WeekIndexes2013.weekTwelve)
		else if (week == 12) newStats.slice(WeekIndexes2013.weekTwelve,  WeekIndexes2013.weekThirteen)
		else if (week == 13) newStats.slice(WeekIndexes2013.weekThirteen,  WeekIndexes2013.weekFourteen)
		else if (week == 14) newStats.slice(WeekIndexes2013.weekFourteen,  WeekIndexes2013.weekFifteen)
		else if (week == 15) newStats.slice(WeekIndexes2013.weekFifteen,  WeekIndexes2013.weekSixteen)
		else if (week == 16) newStats.slice(WeekIndexes2013.weekSixteen,  WeekIndexes2013.weekSeventeen)
		else if (week == 17) newStats.slice(WeekIndexes2013.weekSeventeen,  newStats.size)
		else List(List())
	}


	// def get2014TeamStats(team: String, week: Int): List[String] = {
	// 	if (week == 1){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekOne, WeekIndexes2014.weekTwo)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 2){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekTwo, WeekIndexes2014.weekThree)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 3){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekThree, WeekIndexes2014.weekFour)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 4){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekFour, WeekIndexes2014.weekFive)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 5){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekFive, WeekIndexes2014.weekSix)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 6){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekSix, WeekIndexes2014.weekSeven)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 7){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekSeven, WeekIndexes2014.weekEight)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 8){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekEight, WeekIndexes2014.weekNine)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 9){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekNine, WeekIndexes2014.weekTen)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 10){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekTen, WeekIndexes2014.weekEleven)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 11){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekEleven, WeekIndexes2014.weekTwelve)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 12){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekTwelve, WeekIndexes2014.weekThirteen)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 13){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekThirteen, WeekIndexes2014.weekFourteen)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 14){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekFourteen, WeekIndexes2014.weekFifteen)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 15){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekFifteen, WeekIndexes2014.weekSixteen)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 16){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekSixteen, WeekIndexes2014.weekSeventeen)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else if (week == 17){
	//    		val gamesInWeek =  statsList2014.slice(WeekIndexes2014.weekSeventeen, statsList2014.size)
	//     	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
	//     	if (startIndex == -1) List()
	// 		else gamesInWeek(startIndex)
	//     }
	//     else List[String]()
	// }
		def get2013TeamStats(team:String, week: Int, newStats: List[(String, Double)]): (String, Double) = {
			if (week == 1){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekOne, WeekIndexes2013.weekTwo)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 2){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekTwo, WeekIndexes2013.weekThree)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 3){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekThree, WeekIndexes2013.weekFour)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 4){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekFour, WeekIndexes2013.weekFive)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 5){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekFive, WeekIndexes2013.weekSix)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 6){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekSix, WeekIndexes2013.weekSeven)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 7){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekSeven, WeekIndexes2013.weekEight)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 8){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekEight, WeekIndexes2013.weekNine)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 9){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekNine, WeekIndexes2013.weekTen)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 10){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekTen, WeekIndexes2013.weekEleven)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 11){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekEleven, WeekIndexes2013.weekTwelve)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 12){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekTwelve, WeekIndexes2013.weekThirteen)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 13){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekThirteen, WeekIndexes2013.weekFourteen)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 14){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekFourteen, WeekIndexes2013.weekFifteen)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 15){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekFifteen, WeekIndexes2013.weekSixteen)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 16){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekSixteen, WeekIndexes2013.weekSeventeen)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else if (week == 17){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekSeventeen, newStats.size)
		    	val startIndex = gamesInWeek.indexWhere(x => x(1) == team)
		    	if (startIndex == -1) List()
				else gamesInWeek(startIndex)
		    }
		    else List[String]()
	}	
}