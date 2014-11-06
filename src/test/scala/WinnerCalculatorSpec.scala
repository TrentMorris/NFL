
package trent.nfl

import org.specs2.mutable.Specification
import scala.io.Source

object WinnerCalculatorSpecSpec extends Specification with WinnerCalculator {

    val newStats = modifiedWholeStatsFile(Chromosome.basicChromosome(35))
    "sumGameScore" should {
        "not modify stats that matter" >> {
            val game = statsList2013(1)
            val gameMod = sumGameScore(game, Chromosome.basicChromosome(35))
            gameMod === 1453.0
        }
        "return 0.0 if 0 chromosome" >> {
            val game = statsList2013(1)
            val gameMod = sumGameScore(game, Chromosome.allZeroChromosome(35))
            gameMod === 0.0
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