
package trent.nfl

import org.specs2.mutable.Specification
import scala.io.Source

object WinnerCalculatorSpecSpec extends Specification with WinnerCalculator {

    val newStats = modifiedWholeStatsFile(Chromosome.basicChromosome(35))

    "newCalculateTotalTeamScore" should {
        "have total of 0 if chromosome is zeroes" >> {
            val stats = modifiedWholeStatsFile(Chromosome.allZeroChromosome(35))
            val games = lastNGames2013("Pittsburgh Steelers", 17,16, stats)
            newCalculateTotalTeamScore(games) === 0.0
        }
        "have correct total for basic chromosome" >> {
            val games = lastNGames2013("Miami Dolphins", 4,3, newStats)
            val score1 = get2013TeamStats("Miami Dolphins",1,newStats)._3 + get2013TeamStats("Miami Dolphins",2,newStats)._3 + get2013TeamStats("Miami Dolphins",3,newStats)._3
            score1 === newCalculateTotalTeamScore(games) 
        }
    }

    "newGetMatchups" should {
        "get correct matchups for week 1" >> {
            val weekStats = getNthWeek2013(1, newStats)
            val  matchups = newGetMatchups(weekStats) 
            matchups must contain(("Baltimore Ravens","Denver Broncos"))
            matchups must contain(("Arizona Cardinals", "St Louis Rams"))
            matchups must contain(("Atlanta Falcons","New Orleans Saints"))
            matchups must contain(("Buffalo Bills","New England Patriots"))
            matchups must contain(("Carolina Panthers","Seattle Seahawks"))
            matchups must contain(("Chicago Bears", "Cincinnati Bengals"))
            matchups must contain(("Cleveland Browns","Miami Dolphins"))
            matchups must contain(("Dallas Cowboys","New York Giants"))
            matchups must contain(("Detroit Lions", "Minnesota Vikings"))
            matchups must contain(("Green Bay Packers","San Francisco 49ers"))
            matchups must contain(("Indianapolis Colts", "Oakland Raiders"))
            matchups must contain(("Jacksonville Jaguars","Kansas City Chiefs"))
            matchups must contain(("New York Jets","Tampa Bay Buccaneers"))
            matchups must contain(("Pittsburgh Steelers","Tennessee Titans"))
            matchups must contain(("Philadelphia Eagles","Washington Redskins"))
            matchups must contain(("Houston Texans","San Diego Chargers"))
        }
        "get correct matchups for week 8" >> {
            val weekStats = getNthWeek2013(8, newStats)
            val  matchups = newGetMatchups(weekStats) 
            matchups must contain(("Carolina Panthers", "Tampa Bay Buccaneers"))
            matchups must contain(("Seattle Seahawks", "St Louis Rams"))
        }

        "get correct matchups for week 17" >> {
            val weekStats = getNthWeek2013(17, newStats)
            val  matchups = newGetMatchups(weekStats) 
            matchups must contain(("Arizona Cardinals", "San Francisco 49ers"))
            matchups must contain(("New York Giants", "Washington Redskins"))
        }
    }

    "calculateWinner" should {
        "return correct winner" >> {
            val winner = calculateWinner2013("Pittsburgh Steelers", "Cleveland Browns", 6,3,newStats )
            winner === "Pittsburgh Steelers"
        }
        "return t1 if a tie" >> {
            val stats = modifiedWholeStatsFile(Chromosome.allZeroChromosome(35))
            val winner = calculateWinner2013("Pittsburgh Steelers", "Cleveland Browns", 6,3,stats )
            winner === "Pittsburgh Steelers"        
        }
    }
}