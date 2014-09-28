package trent.nfl

import org.specs2.mutable.Specification
import scala.io.Source

object StatisticalMethodsSpec extends Specification with WinnerCalculator {

    val source = Source.fromURL(getClass.getResource("/nfl2013stats.csv"))
    val stats2013 = source.mkString.replace("\n",",").split(",").toList.sliding(35,35).toList
    val source2014 = Source.fromURL(getClass.getResource("/nfl2014stats.csv"))
    val stats2014 = source2014.mkString.replace("\n",",").split(",").toList.sliding(35,35).toList 

    "getTeamStats for 2013 " should {
        "return right first and last teams in DB for week 1" >> {
            val t1Stats = getTeamStats("Baltimore Ravens", 1,"2013", stats2013)
            t1Stats === List("09/05/2013", "Baltimore Ravens", "27", "24", "36%", "21", "58", "62", "34", "335", "2", "0", "4", "27", "53", "33:48", "50.7", "Denver Broncos", "49", "24", "53%", "23", "65", "42", "27", "445", "0", "2", "3", "17", "61", "26:12", "V", "-7.5", "49.5")
            val t2Stats = getTeamStats("Washington Redskins", 1, "2013",stats2013)
            t2Stats === List("09/09/2013", "Washington Redskins", "27", "25", "20%", "18", "74", "49", "30", "308", "2", "1", "3", "21", "75", "27:21", "42", "Philadelphia Eagles", "33", "26", "47%", "49", "263", "25", "15", "180", "0", "2", "3", "23", "65", "32:39", "H", "4.5", "53.5")
        }
        "return right first and last teams in DB for week 2" >> {
            val t1Stats = getTeamStats("New England Patriots", 2, "2013",stats2013)
            t1Stats === List("09/12/2013", "New England Patriots", "13", "9", "22%", "24", "54", "39", "19", "178", "0", "0", "1", "7", "30", "26:00", "46.7", "New York Jets", "10", "15", "33%", "32", "129", "35", "15", "189", "3", "1", "4", "25", "66", "34:00", "H", "11.5", "43.5")
            val t2Stats = getTeamStats("Pittsburgh Steelers", 2, "2013",stats2013)
            t2Stats === List("09/16/2013", "Pittsburgh Steelers", "10", "14", "25%", "16", "43", "37", "20", "234", "1", "1", "2", "17", "29", "24:27", "46.7", "Cincinnati Bengals", "20", "22", "41%", "34", "127", "45", "25", "280", "0", "0", "0", "0", "84", "35:33", "V", "-6.5", "41")
        }   
        "return right first and last teams in DB for week 3" >> {
            val t1Stats = getTeamStats("Kansas City Chiefs", 3,"2013", stats2013)
            t1Stats === List("09/19/2013", "Kansas City Chiefs", "26", "19", "32%", "38", "146", "35", "22", "248", "0", "0", "5", "25", "65", "39:07", "43.6", "Philadelphia Eagles", "16", "21", "30%", "27", "264", "30", "13", "167", "2", "3", "6", "34", "45", "20:53", "V", "-3.5", "51")
            val t2Stats = getTeamStats("Oakland Raiders", 3, "2013",stats2013)
            t2Stats === List("09/23/2013", "Oakland Raiders", "21", "13", "42%", "17", "49", "31", "21", "293", "0", "0", "3", "23", "77", "24:36", "52.5", "Denver Broncos", "37", "31", "56%", "35", "164", "37", "32", "372", "0", "2", "1", "2", "40", "35:24", "V", "-16.5", "50")
        }
        "return right first and last teams in DB for week 4" >> {
            val t1Stats = getTeamStats("San Francisco 49ers", 4, "2013",stats2013)
            t1Stats === List("09/26/2013", "San Francisco 49ers", "35", "19", "40%", "40", "219", "23", "15", "151", "0", "2", "2", "16", "85", "31:45", "54.6", "St Louis Rams", "11", "14", "18%", "20", "23", "40", "18", "165", "1", "1", "5", "32", "82", "28:15", "V", "4", "43.5")
            val t2Stats = getTeamStats("New Orleans Saints", 4, "2013",stats2013)
            t2Stats === List("09/30/2013", "New Orleans Saints", "38", "23", "46%", "24", "68", "39", "30", "397", "0", "1", "2", "16", "45", "33:47", "47.7", "Miami Dolphins", "17", "19", "33%", "19", "115", "35", "22", "216", "3", "1", "4", "33", "25", "26:13", "H", "7.5", "49")
        }
        "return right first and last teams in DB for week 5" >> {
            val t1Stats = getTeamStats("Buffalo Bills", 5, "2013",stats2013)
            t1Stats === List("10/03/2013", "Buffalo Bills", "24", "20", "31%", "31", "155", "40", "19", "188", "1", "0", "4", "21", "56", "27:37", "45.5", "Cleveland Browns", "37", "19", "41%", "32", "91", "28", "15", "199", "0", "0", "5", "23", "66", "32:23", "V", "-4", "41")
            val t2Stats = getTeamStats("New York Jets", 5, "2013",stats2013)
            t2Stats=== List("10/07/2013", "New York Jets", "30", "15", "55%", "22", "118", "20", "16", "170", "0", "0", "4", "29", "24", "24:34", "46.7", "Atlanta Falcons", "28", "26", "50%", "22", "64", "46", "37", "299", "0", "1", "2", "12", "46", "35:26", "V", "-10.5", "45")
        }
        "return right first and last teams in DB for week 6" >> {
            val t1Stats = getTeamStats("Chicago Bears", 6, "2013",stats2013)
            t1Stats === List("10/10/2013", "Chicago Bears", "27", "26", "45%", "29", "110", "36", "24", "262", "0", "0", "0", "0", "51", "32:54", "37", "New York Giants", "21", "21", "64%", "27", "121", "26", "14", "234", "3", "0", "0", "5", "31", "27:06", "H", "8.5", "48")
            val t2Stats = getTeamStats("San Diego Chargers", 6, "2013",stats2013)
            t2Stats === List("10/14/2013", "San Diego Chargers", "19", "24", "50%", "37", "151", "32", "21", "227", "0", "0", "2", "10", "25", "38:30", "48", "Indianapolis Colts", "9", "12", "20%", "17", "74", "30", "18", "193", "1", "0", "1", "9", "28", "21:30", "H", "0", "50.5")
        }
        "return right first and last teams in DB for week 7" >> {
            val t1Stats = getTeamStats("Arizona Cardinals", 7, "2013",stats2013)
            t1Stats === List("10/17/2013", "Arizona Cardinals", "22", "22", "33%", "18", "30", "45", "30", "204", "2", "0", "7", "54", "22", "32:12", "49", "Seattle Seahawks", "34", "21", "58%", "32", "135", "29", "18", "209", "0", "2", "3", "26", "70", "27:48", "H", "-4.5", "41.5")
            val t2Stats = getTeamStats("New York Giants", 7, "2013",stats2013)
            t2Stats=== List("10/21/2013", "New York Giants", "23", "17", "37%", "32", "64", "39", "23", "193", "0", "1", "2", "7", "72", "36:20", "48.8", "Minnesota Vikings", "7", "13", "39%", "14", "30", "53", "20", "176", "1", "2", "1", "14", "38", "23:40", "H", "3.5", "47")
        }
        "return right first and last teams in DB for week 8" >> {
            val t1Stats = getTeamStats("Carolina Panthers", 8, "2013",stats2013)
            t1Stats === List("10/24/2013", "Carolina Panthers", "31", "21", "42%", "27", "129", "32", "23", "195", "0", "0", "3", "26", "59", "34:34", "43.4", "Tampa Bay Buccaneers", "13", "19", "29%", "14", "48", "51", "30", "249", "0", "1", "3", "26", "21", "25:26", "V", "6.5", "38.5")
            val t2Stats = getTeamStats("St Louis Rams", 8,"2013", stats2013)
            t2Stats=== List("10/28/2013", "St Louis Rams", "9", "23", "36%", "37", "200", "31", "15", "139", "2", "0", "3", "19", "53", "37:58", "46", "Seattle Seahawks", "14", "7", "18%", "15", "44", "18", "10", "91", "0", "0", "7", "48", "83", "22:02", "H", "-13", "43.5")
        }
        "return right first and last teams in DB for week 9" >> {
            val t1Stats = getTeamStats("Cincinnati Bengals", 9, "2013",stats2013)
            t1Stats === List("10/31/2013", "Cincinnati Bengals", "20", "28", "50%", "35", "163", "53", "32", "302", "3", "1", "5", "36", "63", "35:34", "39.5", "Miami Dolphins", "22", "15", "21%", "30", "157", "28", "20", "188", "0", "1", "3", "20", "25", "24:26", "V", "3", "42")
            val t2Stats = getTeamStats("Green Bay Packers", 9, "2013",stats2013)
            t2Stats === List("11/04/2013", "Green Bay Packers", "20", "17", "11%", "29", "199", "21", "12", "113", "1", "0", "5", "28", "0", "26:52", "41.8", "Chicago Bears", "27", "25", "43%", "33", "171", "41", "22", "271", "0", "0", "1", "1", "45", "33:08", "H", "9.5", "50.5")
        }
        "return right first and last teams in DB for week 10" >> {
            val t1Stats = getTeamStats("Minnesota Vikings", 10, "2013",stats2013)
            t1Stats === List("11/07/2013", "Minnesota Vikings", "34", "22", "60%", "24", "91", "27", "21", "216", "1", "0", "1", "5", "7", "24:01", "49", "Washington Redskins", "27", "27", "56%", "36", "191", "37", "24", "242", "0", "0", "4", "39", "63", "35:59", "H", "-1", "48")
            val t2Stats = getTeamStats("Tampa Bay Buccaneers", 10, "2013",stats2013)
            t2Stats=== List("11/11/2013", "Tampa Bay Buccaneers", "22", "18", "25%", "37", "140", "21", "11", "124", "1", "0", "2", "15", "70", "34:41", "43", "Miami Dolphins", "19", "16", "33%", "14", "2", "42", "27", "211", "1", "0", "2", "18", "70", "25:19", "H", "-2.5", "39.5")
        }
        "return right first and last teams in DB for week 11" >> {
            val t1Stats = getTeamStats("Indianapolis Colts", 11,"2013", stats2013)
            t1Stats === List("11/14/2013", "Indianapolis Colts", "30", "24", "46%", "32", "137", "36", "23", "229", "0", "0", "1", "3", "51", "32:23", "36.3", "Tennessee Titans", "27", "20", "55%", "24", "122", "28", "22", "218", "0", "1", "2", "4", "34", "27:37", "V", "3", "42")
            val t2Stats = getTeamStats("New England Patriots", 11, "2013",stats2013)
            t2Stats === List("11/18/2013", "New England Patriots", "20", "28", "50%", "25", "107", "40", "29", "283", "1", "1", "2", "13", "50", "30:48", "31", "Carolina Panthers", "24", "20", "73%", "23", "103", "28", "19", "197", "0", "0", "3", "12", "47", "29:12", "V", "-3", "45.5")
        }
        "return right first and last teams in DB for week 12" >> {
            val t1Stats = getTeamStats("Atlanta Falcons", 12, "2013",stats2013)
            t1Stats === List("11/21/2013", "Atlanta Falcons", "13", "22", "50%", "22", "91", "39", "30", "264", "0", "1", "5", "28", "25", "33:46", "44.3", "New Orleans Saints", "17", "19", "54%", "25", "103", "33", "23", "271", "0", "0", "1", "7", "66", "26:14", "H", "-7", "52.5")
            val t2Stats = getTeamStats("Washington Redskins", 12, "2013",stats2013)
            t2Stats === List("11/25/2013", "Washington Redskins", "6", "11", "27%", "27", "100", "27", "17", "90", "1", "0", "4", "37", "30", "28:28", "45.1", "San Francisco 49ers", "27", "15", "29%", "33", "76", "24", "15", "228", "0", "1", "2", "7", "25", "31:32", "H", "-5", "47")
        }
        "return right first and last teams in DB for week 13" >> {
            val t1Stats = getTeamStats("Baltimore Ravens", 13, "2013",stats2013)
            t1Stats === List("11/28/2013", "Baltimore Ravens", "22", "16", "59%", "25", "74", "35", "24", "237", "0", "0", "2", "14", "55", "29:53", "25", "Pittsburgh Steelers", "20", "22", "54%", "18", "72", "44", "28", "257", "0", "0", "0", "0", "51", "30:07", "H", "3", "40.5")
            val t2Stats = getTeamStats("Seattle Seahawks", 13, "2013",stats2013)
            t2Stats === List("12/02/2013", "Seattle Seahawks", "34", "22", "50%", "38", "127", "30", "22", "302", "0", "0", "1", "8", "66", "33:44", "40", "New Orleans Saints", "7", "12", "40%", "17", "44", "38", "23", "144", "0", "1", "1", "3", "52", "26:16", "H", "5", "47.5")
        }
        "return right first and last teams in DB for week 14" >> {
            val t1Stats = getTeamStats("Houston Texans", 14, "2013",stats2013)
            t1Stats === List("12/05/2013", "Houston Texans", "20", "24", "47%", "19", "83", "58", "33", "323", "2", "0", "3", "34", "177", "35:21", "43", "Jacksonville Jaguars", "27", "18", "29%", "28", "149", "28", "13", "132", "0", "0", "1", "6", "57", "24:39", "V", "3.5", "43")
            val t2Stats = getTeamStats("Dallas Cowboys", 14, "2013",stats2013)
            t2Stats === List("12/09/2013", "Dallas Cowboys", "28", "23", "50%", "28", "198", "25", "14", "130", "0", "0", "2", "14", "50", "23:21", "38.3", "Chicago Bears", "45", "34", "73%", "32", "149", "36", "27", "341", "0", "0", "1", "7", "15", "36:39", "V", "-1.5", "49.5")
        }
        "return right first and last teams in DB for week 15" >> {
            val t1Stats = getTeamStats("Denver Broncos", 15, "2013",stats2013)
            t1Stats === List("12/12/2013", "Denver Broncos", "20", "19", "22%", "11", "18", "41", "27", "277", "1", "0", "1", "12", "43", "21:21", "47", "San Diego Chargers", "27", "25", "50%", "44", "177", "20", "12", "160", "0", "0", "2", "6", "35", "38:39", "H", "9.5", "57")
            val t2Stats = getTeamStats("Detroit Lions", 15, "2013",stats2013)
            t2Stats === List("12/16/2013", "Detroit Lions", "16", "19", "31%", "28", "119", "34", "18", "233", "3", "0", "1", "5", "89", "32:21", "46", "Baltimore Ravens", "18", "18", "29%", "21", "90", "38", "20", "215", "0", "0", "1", "7", "60", "27:39", "H", "5.5", "49")
        }
        "return right first and last teams in DB for week 16" >> {
            val t1Stats = getTeamStats("Arizona Cardinals", 16, "2013",stats2013)
            t1Stats === List("12/22/2013", "Arizona Cardinals", "17", "17", "32%", "43", "139", "25", "13", "168", "4", "0", "2", "10", "46", "37:17", "49.8", "Seattle Seahawks", "10", "10", "15%", "20", "103", "27", "11", "89", "1", "1", "4", "19", "102", "22:43", "V", "-9", "43")
            val t2Stats = getTeamStats("San Francisco 49ers", 16, "2013",stats2013)
            t2Stats === List("12/23/2013", "San Francisco 49ers", "34", "21", "44%", "30", "199", "21", "13", "180", "0", "0", "3", "17", "45", "29:34", "53.3", "Atlanta Falcons", "24", "27", "53%", "20", "61", "48", "37", "341", "2", "0", "1", "7", "37", "30:26", "H", "14.5", "46")
        }
        "return right first and last teams in DB for week 17" >> {
            val t1Stats = getTeamStats("Arizona Cardinals", 17, "2013",stats2013)
            t1Stats === List("12/29/2013", "Arizona Cardinals", "20", "19", "27%", "22", "83", "49", "28", "399", "1", "1", "1", "8", "30", "32:10", "48.3", "San Francisco 49ers", "23", "19", "23%", "23", "83", "34", "21", "292", "0", "0", "2", "18", "20", "27:50", "H", "3", "41")
            val t2Stats = getTeamStats("Washington Redskins", 17, "2013",stats2013)
            t2Stats === List("12/29/2013", "Washington Redskins", "6", "13", "25%", "23", "91", "49", "19", "160", "2", "2", "3", "9", "30", "28:27", "40.3", "New York Giants", "20", "15", "21%", "35", "122", "32", "12", "156", "2", "1", "1", "7", "20", "31:48", "V", "-3.5", "44.5")
        }
        "return empty list when not a real team name" >> {
            getTeamStats("FakeTeam", 17, "2013",stats2013) === List()
        }
        "return empty list if not a real week" >> {
            getTeamStats("Pittsburgh Steelers", 18,"2013", stats2013) === List()
        }
    }
    
    "getTeamStats for 2014" should {
        "return right first and last teams in DB for week 1" >> {
            val t1Stats = getTeamStats("Green Bay Packers", 1,"2014", stats2014)
            t1Stats === List("09/04/2014","Green Bay Packers","16","19","50%","21","80","33","23","175","1","0","3","14","65","26:40","38.3","Seattle Seahawks","36","25","36%","37","207","28","19","191","0","1","1","0","69","33:20","V","-5.5","46.5")
            val t2Stats = getTeamStats("San Diego Chargers", 1, "2014",stats2014)
            t2Stats === List("09/08/2014","San Diego Chargers","17","15","40%","24","52","36","21","238","1","0","0","0","47","28:11","39.3","Arizona Cardinals","18","15","46%","26","109","37","24","294","0","2","2","10","23","31:49","V","-3","47")
        }
        // "return right first and last teams in DB for week 2" >> {
        //     val t1Stats = getTeamStats("New England Patriots", 2, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = getTeamStats("Pittsburgh Steelers", 2, "2014",stats2014)
        //     t2Stats = List()
        // }   
        // "return right first and last teams in DB for week 3" >> {
        //     val t1Stats = getTeamStats("Kansas City Chiefs", 3,"2014", stats2014)
        //     t1Stats === List()
        //     val t2Stats = getTeamStats("Oakland Raiders", 3, "2014",stats2014)
        //     t2Stats === List()
        // }
        // "return right first and last teams in DB for week 4" >> {
        //     val t1Stats = getTeamStats("San Francisco 49ers", 4, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = getTeamStats("New Orleans Saints", 4, "2014",stats2014)
        //     t2Stats === List()
        // }
        // "return right first and last teams in DB for week 5" >> {
        //     val t1Stats = getTeamStats("Buffalo Bills", 5, "2014",stats2014)
        //     t1Stats === List()
        //     val t2Stats = getTeamStats("New York Jets", 5, "2014",stats2014)
        //     t1Stats === List()
        // }
        // "return right first and last teams in DB for week 6" >> {
        //     val t1Stats = getTeamStats("Chicago Bears", 6, "2014",stats2014)
        //     t1Stats === List("10/10/2014", "Chicago Bears", "27", "26", "45%", "29", "110", "36", "24", "262", "0", "0", "0", "0", "51", "32:54", "37", "New York Giants", "21", "21", "64%", "27", "121", "26", "14", "234", "3", "0", "0", "5", "31", "27:06", "H", "8.5", "48")
        //     val t2Stats = getTeamStats("San Diego Chargers", 6, "2014",stats2014)
        //     t2Stats === List("10/14/2014", "San Diego Chargers", "19", "24", "50%", "37", "151", "32", "21", "227", "0", "0", "2", "10", "25", "38:30", "48", "Indianapolis Colts", "9", "12", "20%", "17", "74", "30", "18", "193", "1", "0", "1", "9", "28", "21:30", "H", "0", "50.5")
        // }
        // "return right first and last teams in DB for week 7" >> {
        //     val t1Stats = getTeamStats("Arizona Cardinals", 7, "2014",stats2014)
        //     t1Stats === List("10/17/2014", "Arizona Cardinals", "22", "22", "33%", "18", "30", "45", "30", "204", "2", "0", "7", "54", "22", "32:12", "49", "Seattle Seahawks", "34", "21", "58%", "32", "135", "29", "18", "209", "0", "2", "3", "26", "70", "27:48", "H", "-4.5", "41.5")
        //     val t2Stats = getTeamStats("New York Giants", 7, "2014",stats2014)
        //     t2Stats=== List("10/21/2014", "New York Giants", "23", "17", "37%", "32", "64", "39", "23", "193", "0", "1", "2", "7", "72", "36:20", "48.8", "Minnesota Vikings", "7", "13", "39%", "14", "30", "53", "20", "176", "1", "2", "1", "14", "38", "23:40", "H", "3.5", "47")
        // }
        // "return right first and last teams in DB for week 8" >> {
        //     val t1Stats = getTeamStats("Carolina Panthers", 8, "2014",stats2014)
        //     t1Stats === List("10/24/2014", "Carolina Panthers", "31", "21", "42%", "27", "129", "32", "23", "195", "0", "0", "3", "26", "59", "34:34", "43.4", "Tampa Bay Buccaneers", "13", "19", "29%", "14", "48", "51", "30", "249", "0", "1", "3", "26", "21", "25:26", "V", "6.5", "38.5")
        //     val t2Stats = getTeamStats("St Louis Rams", 8,"2014", stats2014)
        //     t2Stats=== List("10/28/2014", "St Louis Rams", "9", "23", "36%", "37", "200", "31", "15", "139", "2", "0", "3", "19", "53", "37:58", "46", "Seattle Seahawks", "14", "7", "18%", "15", "44", "18", "10", "91", "0", "0", "7", "48", "83", "22:02", "H", "-13", "43.5")
        // }
        // "return right first and last teams in DB for week 9" >> {
        //     val t1Stats = getTeamStats("Cincinnati Bengals", 9, "2014",stats2014)
        //     t1Stats === List("10/31/2014", "Cincinnati Bengals", "20", "28", "50%", "35", "163", "53", "32", "302", "3", "1", "5", "36", "63", "35:34", "39.5", "Miami Dolphins", "22", "15", "21%", "30", "157", "28", "20", "188", "0", "1", "3", "20", "25", "24:26", "V", "3", "42")
        //     val t2Stats = getTeamStats("Green Bay Packers", 9, "2014",stats2014)
        //     t2Stats === List("11/04/2014", "Green Bay Packers", "20", "17", "11%", "29", "199", "21", "12", "113", "1", "0", "5", "28", "0", "26:52", "41.8", "Chicago Bears", "27", "25", "43%", "33", "171", "41", "22", "271", "0", "0", "1", "1", "45", "33:08", "H", "9.5", "50.5")
        // }
        // "return right first and last teams in DB for week 10" >> {
        //     val t1Stats = getTeamStats("Minnesota Vikings", 10, "2014",stats2014)
        //     t1Stats === List("11/07/2014", "Minnesota Vikings", "34", "22", "60%", "24", "91", "27", "21", "216", "1", "0", "1", "5", "7", "24:01", "49", "Washington Redskins", "27", "27", "56%", "36", "191", "37", "24", "242", "0", "0", "4", "39", "63", "35:59", "H", "-1", "48")
        //     val t2Stats = getTeamStats("Tampa Bay Buccaneers", 10, "2014",stats2014)
        //     t2Stats=== List("11/11/2014", "Tampa Bay Buccaneers", "22", "18", "25%", "37", "140", "21", "11", "124", "1", "0", "2", "15", "70", "34:41", "43", "Miami Dolphins", "19", "16", "33%", "14", "2", "42", "27", "211", "1", "0", "2", "18", "70", "25:19", "H", "-2.5", "39.5")
        // }
        // "return right first and last teams in DB for week 11" >> {
        //     val t1Stats = getTeamStats("Indianapolis Colts", 11,"2014", stats2014)
        //     t1Stats === List("11/14/2014", "Indianapolis Colts", "30", "24", "46%", "32", "137", "36", "23", "229", "0", "0", "1", "3", "51", "32:23", "36.3", "Tennessee Titans", "27", "20", "55%", "24", "122", "28", "22", "218", "0", "1", "2", "4", "34", "27:37", "V", "3", "42")
        //     val t2Stats = getTeamStats("New England Patriots", 11, "2014",stats2014)
        //     t2Stats === List("11/18/2014", "New England Patriots", "20", "28", "50%", "25", "107", "40", "29", "283", "1", "1", "2", "13", "50", "30:48", "31", "Carolina Panthers", "24", "20", "73%", "23", "103", "28", "19", "197", "0", "0", "3", "12", "47", "29:12", "V", "-3", "45.5")
        // }
        // "return right first and last teams in DB for week 12" >> {
        //     val t1Stats = getTeamStats("Atlanta Falcons", 12, "2014",stats2014)
        //     t1Stats === List("11/21/2014", "Atlanta Falcons", "13", "22", "50%", "22", "91", "39", "30", "264", "0", "1", "5", "28", "25", "33:46", "44.3", "New Orleans Saints", "17", "19", "54%", "25", "103", "33", "23", "271", "0", "0", "1", "7", "66", "26:14", "H", "-7", "52.5")
        //     val t2Stats = getTeamStats("Washington Redskins", 12, "2014",stats2014)
        //     t2Stats === List("11/25/2014", "Washington Redskins", "6", "11", "27%", "27", "100", "27", "17", "90", "1", "0", "4", "37", "30", "28:28", "45.1", "San Francisco 49ers", "27", "15", "29%", "33", "76", "24", "15", "228", "0", "1", "2", "7", "25", "31:32", "H", "-5", "47")
        // }
        // "return right first and last teams in DB for week 13" >> {
        //     val t1Stats = getTeamStats("Baltimore Ravens", 13, "2014",stats2014)
        //     t1Stats === List("11/28/2014", "Baltimore Ravens", "22", "16", "59%", "25", "74", "35", "24", "237", "0", "0", "2", "14", "55", "29:53", "25", "Pittsburgh Steelers", "20", "22", "54%", "18", "72", "44", "28", "257", "0", "0", "0", "0", "51", "30:07", "H", "3", "40.5")
        //     val t2Stats = getTeamStats("Seattle Seahawks", 13, "2014",stats2014)
        //     t2Stats === List("12/02/2014", "Seattle Seahawks", "34", "22", "50%", "38", "127", "30", "22", "302", "0", "0", "1", "8", "66", "33:44", "40", "New Orleans Saints", "7", "12", "40%", "17", "44", "38", "23", "144", "0", "1", "1", "3", "52", "26:16", "H", "5", "47.5")
        // }
        // "return right first and last teams in DB for week 14" >> {
        //     val t1Stats = getTeamStats("Houston Texans", 14, "2014",stats2014)
        //     t1Stats === List("12/05/2014", "Houston Texans", "20", "24", "47%", "19", "83", "58", "33", "323", "2", "0", "3", "34", "177", "35:21", "43", "Jacksonville Jaguars", "27", "18", "29%", "28", "149", "28", "13", "132", "0", "0", "1", "6", "57", "24:39", "V", "3.5", "43")
        //     val t2Stats = getTeamStats("Dallas Cowboys", 14, "2014",stats2014)
        //     t2Stats === List("12/09/2014", "Dallas Cowboys", "28", "23", "50%", "28", "198", "25", "14", "130", "0", "0", "2", "14", "50", "23:21", "38.3", "Chicago Bears", "45", "34", "73%", "32", "149", "36", "27", "341", "0", "0", "1", "7", "15", "36:39", "V", "-1.5", "49.5")
        // }
        // "return right first and last teams in DB for week 15" >> {
        //     val t1Stats = getTeamStats("Denver Broncos", 15, "2014",stats2014)
        //     t1Stats === List("12/12/2014", "Denver Broncos", "20", "19", "22%", "11", "18", "41", "27", "277", "1", "0", "1", "12", "43", "21:21", "47", "San Diego Chargers", "27", "25", "50%", "44", "177", "20", "12", "160", "0", "0", "2", "6", "35", "38:39", "H", "9.5", "57")
        //     val t2Stats = getTeamStats("Detroit Lions", 15, "2014",stats2014)
        //     t2Stats === List("12/16/2014", "Detroit Lions", "16", "19", "31%", "28", "119", "34", "18", "233", "3", "0", "1", "5", "89", "32:21", "46", "Baltimore Ravens", "18", "18", "29%", "21", "90", "38", "20", "215", "0", "0", "1", "7", "60", "27:39", "H", "5.5", "49")
        // }
        // "return right first and last teams in DB for week 16" >> {
        //     val t1Stats = getTeamStats("Arizona Cardinals", 16, "2014",stats2014)
        //     t1Stats === List("12/22/2014", "Arizona Cardinals", "17", "17", "32%", "43", "139", "25", "13", "168", "4", "0", "2", "10", "46", "37:17", "49.8", "Seattle Seahawks", "10", "10", "15%", "20", "103", "27", "11", "89", "1", "1", "4", "19", "102", "22:43", "V", "-9", "43")
        //     val t2Stats = getTeamStats("San Francisco 49ers", 16, "2014",stats2014)
        //     t2Stats === List("12/23/2014", "San Francisco 49ers", "34", "21", "44%", "30", "199", "21", "13", "180", "0", "0", "3", "17", "45", "29:34", "53.3", "Atlanta Falcons", "24", "27", "53%", "20", "61", "48", "37", "341", "2", "0", "1", "7", "37", "30:26", "H", "14.5", "46")
        // }
        // "return right first and last teams in DB for week 17" >> {
        //     val t1Stats = getTeamStats("Arizona Cardinals", 17, "2014",stats2014)
        //     t1Stats === List("12/29/2014", "Arizona Cardinals", "20", "19", "27%", "22", "83", "49", "28", "399", "1", "1", "1", "8", "30", "32:10", "48.3", "San Francisco 49ers", "23", "19", "23%", "23", "83", "34", "21", "292", "0", "0", "2", "18", "20", "27:50", "H", "3", "41")
        //     val t2Stats = getTeamStats("Washington Redskins", 17, "2014",stats2014)
        //     t2Stats === List("12/29/2014", "Washington Redskins", "6", "13", "25%", "23", "91", "49", "19", "160", "2", "2", "3", "9", "30", "28:27", "40.3", "New York Giants", "20", "15", "21%", "35", "122", "32", "12", "156", "2", "1", "1", "7", "20", "31:48", "V", "-3.5", "44.5")
        // }
        "return empty list when not a real team name" >> {
            getTeamStats("FakeTeam", 17, "2014",stats2014) === List()
        }
        "return empty list if not a real week" >> {
            getTeamStats("Pittsburgh Steelers", 18,"2014", stats2014) === List()
        }
    }

    "lastNGames2013" should {
        "return right number of games" >> {
            val games = lastNGames2013("Pittsburgh Steelers", 8, 7,stats2013)
            games.size === 7
        }
        "not contain an empty List" >> {
            val games = lastNGames2013("Atlanta Falcons", 17, 16,stats2013)
            games must not contain List()
            games.size === 16
        }
    }
    "modifyStats" should {
        "not modify stats that matter" >> {
            val game = lastNGames2013("Baltimore Ravens", 1,1, stats2013).flatten
            val gameMod = modifyStats(game, new Chromosome(List.fill(35)(1.0.toFloat)))
            gameMod === List(0.0, 0.0, 27.0, 24.0, 36.0, 21.0, 58.0, 62.0, 34.0 ,335.0 ,2.0 ,0.0 ,4.0,27.0,53.0,33.0 ,50.7, 0.0, 49.0, 24.0, 53.0, 23.0, 65.0, 42.0, 27.0, 445.0 ,0.0,2.0, 3.0, 17.0 ,61.0 ,26.0, 0.0, -7.5, 49.5)

        }
    }
    "calculateTotalTeamScore" should {
        "return correct score" >> {
            val game = lastNGames2013("Baltimore Ravens", 1,1, stats2013).flatten
            val gameMod = List(modifyStats(game, new Chromosome(List.fill(35)(1.0.toFloat))))
            val score = calculateTotalTeamScore(gameMod)
            score === 1645.7
        }
        "be zero if chromosome is all zeroes" >>{
            val game = lastNGames2013("Baltimore Ravens", 1,1, stats2013).flatten
            val gameMod = List(modifyStats(game, new Chromosome(List.fill(35)(0.0.toFloat))))
            val score2 = calculateTotalTeamScore(gameMod)
            score2 === 0.0
        }
    }
    "calculateWinner" should {
        "return correct winner" >> {
            val winner = calculateWinner("Pittsburgh Steelers", "Cleveland Browns", new Chromosome(List.fill(35)(1.0.toFloat)), 6,3, stats2013,"2013" )
            winner === "Pittsburgh Steelers"
        }
    }
}