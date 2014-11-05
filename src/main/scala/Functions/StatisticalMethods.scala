package trent.nfl
import scala.io.Source

trait StatisticMethods {

	val source = Source.fromURL(getClass.getResource("/nfl2013stats.csv"))
    val statsList2013 = source.mkString.replace("\n",",").split(",").toList.sliding(35,35).toList

    val secondSource = Source.fromURL(getClass.getResource("/nfl2014stats.csv"))
    val statsList2014 = secondSource.mkString.replace("\n", ",").split(",").toList.sliding(35, 35).toList

    def modifiedWholeStatsFile(ch: Chromosome): List[(String,String, Double, String)] = {
	   	for (game <- statsList2013.slice(1,statsList2013.size + 1)) yield {
	   		val winner = if (game(2).toDouble > game(18).toDouble) game(1)
                   else if (game(2).toDouble < game(18).toDouble) game(19)
                   else game(1)
 			println(winner)
	   		(game(1),game(16), modifyStats(game, ch).sum, winner)
	   	}
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

	def newLastNGames2013(team: String, week: Int, lastNGames: Int, newStats: List[(String,String, Double, String)]): List[(String,String, Double,String)] = {
		val games= for (weekNumber <- List.range(0,lastNGames))  yield( get2013TeamStats(team,(week-weekNumber), newStats))
		if (games.contains(List())){ 
			val newGames = games.filter(x => x != List())
			get2013TeamStats(team, (week - lastNGames), newStats) :: newGames
		}
		else games
	}
	
	def getNthWeek2013(week: Int, newStats: List[(String,String, Double,String)]): List[(String, String, Double, String)] = {
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
		else List(("","",0.0,""))
	}


	// def getNthWeek2014(week: Int, newStats: List[(String,String, Double,String)]): List[(String, String, Double, String)] = {
	// 	if 		(week ==1)	newStats.slice(WeekIndexes2014.weekOne, WeekIndexes2014.weekTwo)
	// 	else if (week == 2) newStats.slice(WeekIndexes2014.weekTwo,  WeekIndexes2014.weekThree)
	// 	else if (week == 3) newStats.slice(WeekIndexes2014.weekThree, WeekIndexes2014.weekFour)
	// 	else if (week == 4) newStats.slice(WeekIndexes2014.weekFour,  WeekIndexes2014.weekFive)
	// 	else if (week == 5) newStats.slice(WeekIndexes2014.weekFive,  WeekIndexes2014.weekSix)
	// 	else if (week == 6) newStats.slice(WeekIndexes2014.weekSix,   WeekIndexes2014.weekSeven)
	// 	else if (week == 7) newStats.slice(WeekIndexes2014.weekSeven, WeekIndexes2014.weekEight)
	// 	else if (week == 8) newStats.slice(WeekIndexes2014.weekEight, WeekIndexes2014.weekNine)
	// 	else if (week == 9) newStats.slice(WeekIndexes2014.weekNine,  WeekIndexes2014.weekTen)
	// 	else if (week == 10) newStats.slice(WeekIndexes2014.weekTen,  WeekIndexes2014.weekEleven)
	// 	else if (week == 11) newStats.slice(WeekIndexes2014.weekEleven,  WeekIndexes2014.weekTwelve)
	// 	else if (week == 12) newStats.slice(WeekIndexes2014.weekTwelve,  WeekIndexes2014.weekThirteen)
	// 	else if (week == 13) newStats.slice(WeekIndexes2014.weekThirteen,  WeekIndexes2014.weekFourteen)
	// 	else if (week == 14) newStats.slice(WeekIndexes2014.weekFourteen,  WeekIndexes2014.weekFifteen)
	// 	else if (week == 15) newStats.slice(WeekIndexes2014.weekFifteen,  WeekIndexes2014.weekSixteen)
	// 	else if (week == 16) newStats.slice(WeekIndexes2014.weekSixteen,  WeekIndexes2014.weekSeventeen)
	// 	else if (week == 17) newStats.slice(WeekIndexes2014.weekSeventeen,  newStats.size)
	// 	else List(("","",0.0,""))
	// }


	// 	def get2013TeamStats(team:String, week: Int, newStats: List[(String,String, Double,String)]): (String,String, Double,String) = {
	// 		if (week == 1){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekOne, WeekIndexes2014.weekTwo)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 2){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekTwo, WeekIndexes2014.weekThree)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 3){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekThree, WeekIndexes2014.weekFour)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 4){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekFour, WeekIndexes2014.weekFive)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 5){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekFive, WeekIndexes2014.weekSix)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 6){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekSix, WeekIndexes2014.weekSeven)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 7){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekSeven, WeekIndexes2014.weekEight)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 8){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekEight, WeekIndexes2014.weekNine)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 9){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekNine, WeekIndexes2014.weekTen)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 10){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekTen, WeekIndexes2014.weekEleven)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 11){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekEleven, WeekIndexes2014.weekTwelve)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 12){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekTwelve, WeekIndexes2014.weekThirteen)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 13){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekThirteen, WeekIndexes2014.weekFourteen)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 14){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekFourteen, WeekIndexes2014.weekFifteen)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 15){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekFifteen, WeekIndexes2014.weekSixteen)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 16){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekSixteen, WeekIndexes2014.weekSeventeen)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else if (week == 17){
	// 	   		val gamesInWeek =  newStats.slice(WeekIndexes2014.weekSeventeen, newStats.size)
	// 	    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
	// 	    	if (startIndex == -1) ("","",0.0,"")
	// 			else gamesInWeek(startIndex)
	// 	    }
	// 	    else ("","",0.0,"")
	// }	
		def get2013TeamStats(team:String, week: Int, newStats: List[(String,String, Double,String)]): (String,String, Double,String) = {
			if (week == 1){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekOne, WeekIndexes2013.weekTwo)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 2){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekTwo, WeekIndexes2013.weekThree)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 3){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekThree, WeekIndexes2013.weekFour)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 4){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekFour, WeekIndexes2013.weekFive)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 5){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekFive, WeekIndexes2013.weekSix)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 6){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekSix, WeekIndexes2013.weekSeven)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 7){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekSeven, WeekIndexes2013.weekEight)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 8){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekEight, WeekIndexes2013.weekNine)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 9){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekNine, WeekIndexes2013.weekTen)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 10){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekTen, WeekIndexes2013.weekEleven)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 11){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekEleven, WeekIndexes2013.weekTwelve)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 12){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekTwelve, WeekIndexes2013.weekThirteen)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 13){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekThirteen, WeekIndexes2013.weekFourteen)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 14){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekFourteen, WeekIndexes2013.weekFifteen)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 15){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekFifteen, WeekIndexes2013.weekSixteen)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 16){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekSixteen, WeekIndexes2013.weekSeventeen)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else if (week == 17){
		   		val gamesInWeek =  newStats.slice(WeekIndexes2013.weekSeventeen, newStats.size)
		    	val startIndex = gamesInWeek.indexWhere(x => x._1 == team)
		    	if (startIndex == -1) ("","",0.0,"")
				else gamesInWeek(startIndex)
		    }
		    else ("","",0.0,"")
	}	
}