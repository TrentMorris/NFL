package trent.nfl

import org.specs2.mutable.Specification
import scala.io.Source

object StatisticalMethodsSpec extends Specification with WinnerCalculator {

    val newStats = modifiedWholeStatsFile(Chromosome.basicChromosome(35))

    "modifiedWholeStatsFile" should {
        val file = modifiedWholeStatsFile(Chromosome.basicChromosome(35))
        file.size === 512
        file(0)._1 === "Baltimore Ravens"
        file(0)._2 === "Denver Broncos"
        file(0)._3 must haveClass[java.lang.Double]
        file(511)._1 === "Washington Redskins"
        file(511)._2 === "New York Giants"
        file(511)._3 must haveClass[java.lang.Double]
    }

    "lastNGames2013" should {
        "have correct number of games" >> {
            lastNGames2013("Pittsburgh Steelers", 16, 8, newStats).size === 8
        }
        "have actual right games" >> {
            val games = lastNGames2013("Pittsburgh Steelers", 3,2,newStats)
            games.size === 2
            games(1)._2 === "Tennessee Titans"
            games(0)._2 === "Cincinnati Bengals"
        }
    }
    "sumGameScore" should {
        "not modify stats that matter" >> {
            val game = statsList2013(1)
            val gameMod = sumGameScore(game, Chromosome.basicChromosome(35))
            gameMod === -21.0
        }
        "return 0.0 if 0 chromosome" >> {
            val game = statsList2013(1)
            val gameMod = sumGameScore(game, Chromosome.allZeroChromosome(35))
            gameMod === 0.0
        }
    }

    "get2013TeamStats" should {
        "return right first and last teams in DB for week 1" >> {
            val t1Stats =  get2013TeamStats("Baltimore Ravens", 1, newStats)
            t1Stats._1 === "Baltimore Ravens"
            t1Stats._2 === "Denver Broncos"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("Washington Redskins", 1, newStats)
            t2Stats._1 === "Washington Redskins"
            t2Stats._2 == "Baltimore Ravens"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 2" >> {
            val t1Stats = get2013TeamStats("New England Patriots", 2, newStats)
            t1Stats._1 === "New England Patriots"
            t1Stats._2 === "New York Jets"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("Pittsburgh Steelers", 2, newStats)
            t2Stats._1 === "Pittsburgh Steelers"
            t2Stats._2 === "Cincinnati Bengals"
            t2Stats._3 must haveClass[java.lang.Double]
        }   
        "return right first and last teams in DB for week 3" >> {
            val t1Stats = get2013TeamStats("Kansas City Chiefs", 3, newStats)
            t1Stats._1 === "Kansas City Chiefs"
            t1Stats._2 === "Philadelphia Eagles"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("Oakland Raiders", 3, newStats)
            t2Stats._1 === "Oakland Raiders"
            t2Stats._2 === "Denver Broncos"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 4" >> {
            val t1Stats = get2013TeamStats("San Francisco 49ers", 4, newStats)
            t1Stats._1 === "San Francisco 49ers"
            t1Stats._2 === "St Louis Rams"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("New Orleans Saints", 4, newStats)
            t2Stats._1 === "New Orleans Saints"
            t2Stats._2 === "Miami Dolphins"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 5" >> {
            val t1Stats = get2013TeamStats("Buffalo Bills", 5, newStats)
            t1Stats._1 === "Buffalo Bills"
            t1Stats._2 === "Cleveland Browns"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("New York Jets", 5, newStats)
            t2Stats._1 === "New York Jets"
            t2Stats._2 === "Atlanta Falcons"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 6" >> {
            val t1Stats = get2013TeamStats("Chicago Bears", 6, newStats)
            t1Stats._1 === "Chicago Bears"
            t1Stats._2 === "New York Giants"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("San Diego Chargers", 6, newStats)
            t2Stats._1 === "San Diego Chargers"
            t2Stats._2 === "Indianapolis Colts"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 7" >> {
            val t1Stats = get2013TeamStats("Arizona Cardinals", 7, newStats)
            t1Stats._1 === "Arizona Cardinals"
            t1Stats._2 === "Seattle Seahawks"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("New York Giants", 7, newStats)
            t2Stats._1 === "New York Giants"
            t2Stats._2 === "Minnesota Vikings"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 8" >> {
            val t1Stats = get2013TeamStats("Carolina Panthers", 8, newStats)
            t1Stats._1 === "Carolina Panthers"
            t1Stats._2 === "Tampa Bay Buccaneers"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("St Louis Rams", 8, newStats)
            t2Stats._1 === "St Louis Rams"
            t2Stats._2 === "Seattle Seahawks"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 9" >> {
            val t1Stats = get2013TeamStats("Cincinnati Bengals", 9, newStats)
            t1Stats._1 ==="Cincinnati Bengals"
            t1Stats._2 === "Miami Dolphins"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("Green Bay Packers", 9, newStats)
            t2Stats._1 === "Green Bay Packers"
            t2Stats._2 === "Chicago Bears"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 10" >> {
            val t1Stats = get2013TeamStats("Minnesota Vikings", 10, newStats)
            t1Stats._1 === "Minnesota Vikings"
            t1Stats._2 === "Washington Redskins"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("Tampa Bay Buccaneers", 10, newStats)
            t2Stats._1 === "Tampa Bay Buccaneers"
            t2Stats._2 === "Miami Dolphins"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 11" >> {
            val t1Stats = get2013TeamStats("Indianapolis Colts", 11, newStats)
            t1Stats._1 === "Indianapolis Colts"
            t1Stats._2 === "Tennessee Titans"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("New England Patriots", 11, newStats)
            t2Stats._1 === "New England Patriots"
            t2Stats._2 === "Carolina Panthers"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 12" >> {
            val t1Stats = get2013TeamStats("Atlanta Falcons", 12, newStats)
            t1Stats._1 === "Atlanta Falcons"
            t1Stats._2 === "New Orleans Saints"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("Washington Redskins", 12, newStats)
            t2Stats._1 === "Washington Redskins"
            t2Stats._2 === "San Francisco 49ers"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 13" >> {
            val t1Stats = get2013TeamStats("Baltimore Ravens", 13, newStats)
            t1Stats._1 === "Baltimore Ravens"
            t1Stats._2 === "Pittsburgh Steelers"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("Seattle Seahawks", 13, newStats)
            t2Stats._1 === "Seattle Seahawks"
            t2Stats._2 === "New Orleans Saints"
            t2Stats._3 must haveClass[java.lang.Double]

        }
        "return right first and last teams in DB for week 14" >> {
            val t1Stats = get2013TeamStats("Houston Texans", 14, newStats)
            t1Stats._1 === "Houston Texans"
            t1Stats._2 === "Jacksonville Jaguars"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("Dallas Cowboys", 14, newStats)
            t2Stats._1 === "Dallas Cowboys"
            t2Stats._2 === "Chicago Bears"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 15" >> {
            val t1Stats = get2013TeamStats("Denver Broncos", 15, newStats)
            t1Stats._1 === "Denver Broncos"
            t1Stats._2 === "San Diego Chargers"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("Detroit Lions", 15, newStats)
            t2Stats._1 === "Detroit Lions"
            t2Stats._2 === "Baltimore Ravens"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 16" >> {
            val t1Stats = get2013TeamStats("Arizona Cardinals", 16, newStats)
            t1Stats._1 === "Arizona Cardinals"
            t1Stats._2 === "Seattle Seahawks"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("San Francisco 49ers", 16, newStats)
            t2Stats._1 === "San Francisco 49ers"
            t2Stats._2 === "Atlanta Falcons"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return right first and last teams in DB for week 17" >> {
            val t1Stats = get2013TeamStats("Arizona Cardinals", 17, newStats)
            t1Stats._1 === "Arizona Cardinals"
            t1Stats._2 === "San Francisco 49ers"
            t1Stats._3 must haveClass[java.lang.Double]

            val t2Stats = get2013TeamStats("Washington Redskins", 17, newStats)
            t2Stats._1 ==="Washington Redskins"
            t2Stats._2 === "New York Giants"
            t2Stats._3 must haveClass[java.lang.Double]
        }
        "return empty list when not a real team name" >> {
            get2013TeamStats("FakeTeam", 17, newStats) === ("","",0.0,"")
        }
        "return empty list if not a real week" >> {
            get2013TeamStats("Pittsburgh Steelers", 18, newStats) === ("","",0.0,"")
        }
    }
    
    // "get2013TeamStats for 2014" should {
    //     "return right first and last teams in DB for week 1" >> {
    //         val t1Stats = get2013TeamStats("Green Bay Packers", 1,"2014")
    //         t1Stats === List("09/04/2014","Green Bay Packers","16","19","50%","21","80","33","23","175","1","0","3","14","65","26:40","38.3","Seattle Seahawks","36","25","36%","37","207","28","19","191","0","1","1","0","69","33:20","V","-5.5","46.5")
    //         val t2Stats = get2013TeamStats("San Diego Chargers", 1, "2014")
    //         t2Stats === List("09/08/2014","San Diego Chargers","17","15","40%","24","52","36","21","238","1","0","0","0","47","28:11","39.3","Arizona Cardinals","18","15","46%","26","109","37","24","294","0","2","2","10","23","31:49","V","-3","47")
    //     }
        // "return right first and last teams in DB for week 2" >> {
        //     val t1Stats = get2013TeamStats("New England Patriots", 2, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("Pittsburgh Steelers", 2, "2014",stats2014)
        //     t2Stats = List()
        // }   
        // "return right first and last teams in DB for week 3" >> {
        //     val t1Stats = get2013TeamStats("Kansas City Chiefs", 3,"2014", stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("Oakland Raiders", 3, "2014",stats2014)
        //     t2Stats === List()
        // }
        // "return right first and last teams in DB for week 4" >> {
        //     val t1Stats = get2013TeamStats("San Francisco 49ers", 4, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("New Orleans Saints", 4, "2014",stats2014)
        //     t2Stats === List()
        // }
        // "return right first and last teams in DB for week 5" >> {
        //     val t1Stats = get2013TeamStats("Buffalo Bills", 5, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("New York Jets", 5, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 6" >> {
        //     val t1Stats = get2013TeamStats("Chicago Bears", 6, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("San Diego Chargers", 6, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 7" >> {
        //     val t1Stats = get2013TeamStats("Arizona Cardinals", 7, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("New York Giants", 7, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 8" >> {
        //     val t1Stats = get2013TeamStats("Carolina Panthers", 8, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("St Louis Rams", 8,"2014", stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 9" >> {
        //     val t1Stats = get2013TeamStats("Cincinnati Bengals", 9, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("Green Bay Packers", 9, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 10" >> {
        //     val t1Stats = get2013TeamStats("Minnesota Vikings", 10, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("Tampa Bay Buccaneers", 10, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 11" >> {
        //     val t1Stats = get2013TeamStats("Indianapolis Colts", 11,"2014", stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("New England Patriots", 11, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 12" >> {
        //     val t1Stats = get2013TeamStats("Atlanta Falcons", 12, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("Washington Redskins", 12, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 13" >> {
        //     val t1Stats = get2013TeamStats("Baltimore Ravens", 13, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("Seattle Seahawks", 13, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 14" >> {
        //     val t1Stats = get2013TeamStats("Houston Texans", 14, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("Dallas Cowboys", 14, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 15" >> {
        //     val t1Stats = get2013TeamStats("Denver Broncos", 15, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("Detroit Lions", 15, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 16" >> {
        //     val t1Stats = get2013TeamStats("Arizona Cardinals", 16, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("San Francisco 49ers", 16, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 17" >> {
        //     val t1Stats = get2013TeamStats("Arizona Cardinals", 17, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = get2013TeamStats("Washington Redskins", 17, "2014",stats2014)
        //     t1Stats === List()
        // }
    //     "return empty list when not a real team name" >> {
    //         get2013TeamStats("FakeTeam", 17, "2014") === ("","",0.0,"")
    //     }
    //     "return empty list if not a real week" >> {
    //         get2013TeamStats("Pittsburgh Steelers", 18,"2014") === ("","",0.0,"")
    //     }
    // }

    // def lastNGames2013(team: String, week: Int, lastNGames: Int, newStats: List[(String,String, java.lang.Double, String)]): List[(String,String, java.lang.Double,String)] = {


    "lastNGames2013" should {
        "return right number of games" >> {
            val games = lastNGames2013("Pittsburgh Steelers", 8, 7,newStats).distinct
            games must not contain (("","",0.0,""))
            games.size === 7
        }
        "not contain an empty List" >> {
            val games = lastNGames2013("Atlanta Falcons", 17, 16,newStats).distinct
            games must not contain (("","",0.0,""))
            games.size === 16
        }
    }
}