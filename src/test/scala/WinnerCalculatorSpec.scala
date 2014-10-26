
package trent.nfl

import org.specs2.mutable.Specification
import scala.io.Source

object WinnerCalculatorSpecSpec extends Specification with WinnerCalculator {
    "modifyStats" should {
        "not modify stats that matter" >> {
            val game = lastNGames2013("Baltimore Ravens", 1,1).flatten
            val gameMod = modifyStats(game, Chromosome.basicChromosome(35))
            val testList = List(0.0, 0.0, 27.0, 24.0, 36.0, 21.0, 58.0, 62.0, 34.0 ,335.0 ,2.0 ,0.0 ,4.0,27.0,53.0,33.0 ,0.0, 0.0, 49.0, 24.0, 53.0, 23.0, 65.0, 42.0, 27.0, 445.0 ,0.0,2.0, 3.0, 17.0 ,61.0 ,26.0, -100.0, 0.0, 0.0)
            gameMod === testList
            gameMod.size === testList.size
        }
    }
    "calculateTotalTeamScore" should {
        "return correct score" >> {
            val game = lastNGames2013("Baltimore Ravens", 1,1).flatten
            val gameMod = List(modifyStats(game, Chromosome.basicChromosome(35)))
            val score = calculateTotalTeamScore(gameMod)
            score === 1453.0
        }
        "be zero if chromosome is all zeroes" >>{
            val game = lastNGames2013("Baltimore Ravens", 1,1).flatten
            val gameMod = List(modifyStats(game, Chromosome.allZeroChromosome(35)))
            val score2 = calculateTotalTeamScore(gameMod)
            score2 === 0.0
        }
    }
    "calculateWinner" should {
        "return correct winner" >> {
            val winner = calculateWinner("Pittsburgh Steelers", "Cleveland Browns", Chromosome.basicChromosome(35), 6,3,"2013" )
            winner === "Cleveland Browns"
        }
        "return t1 if a tie" >> {
            val winner = calculateWinner("Pittsburgh Steelers", "Cleveland Browns", Chromosome.allZeroChromosome(35), 6,3,"2013" )
            winner === "Pittsburgh Steelers"        
        }
    }
}