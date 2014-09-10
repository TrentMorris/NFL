package trent.nfl

object StatisticMethods {

	def getTeamStats(team:String, week: Int, statsList: List[String]): List[String] = {
		if (week == 1){
	   		val weekStats =  statsList.slice(WeekIndexes2013.weekOne, WeekIndexes2013.weekTwo)
	    	val startIndex = weekStats.indexOf(team)
	    	weekStats.slice(startIndex, startIndex + 34)
	    }
	    // else if (week == 2){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekTwo, WeekIndexes2013.weekThree)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 3){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekThree, WeekIndexes2013.weekFour)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 4){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekFour, WeekIndexes2013.weekFive)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 5){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekFive, WeekIndexes2013.weekSix)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 6){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekSix, WeekIndexes2013.weekSeven)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 7){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekSeven, WeekIndexes2013.weekEight)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 8){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekEight, WeekIndexes2013.weekNine)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 9){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekNine, WeekIndexes2013.weekTen)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 10){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekTen, WeekIndexes2013.weekEleven)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 11){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekEleven, WeekIndexes2013.weekTwelve)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 12){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekTwelve, WeekIndexes2013.weekThirteen)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 13){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekThirteen, WeekIndexes2013.weekFourteen)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 14){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekFourteen, WeekIndexes2013.weekFifteen)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 15){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekFifteen, WeekIndexes2013.weekSixteen)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 16){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekSixteen, WeekIndexes2013.weekSeventeen)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    // else if (week == 17){
	   	// 	val weekStats =  statsList.slice(WeekIndexes2013.weekSeventeen, WeekIndexes2013.end)
	    // 	val startIndex = weekStats.indexOf(team)
	    // 	weekStats.slice(startIndex, startIndex + 34)
	    // }
	    else List[String]()
	}

}