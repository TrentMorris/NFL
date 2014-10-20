package trent.nfl

import org.specs2.mutable.Specification
import scala.io.Source

object GeneticAlgorithmScalaSpec extends Specification with WinnerCalculator with GeneticAlgorithmScala {
	val population = Population(1)(35)

	"Chromosome.apply" should {
		"have values between -1 and 1 depending on index" >> {
			val ch = Chromosome(35)
			ch.size === 35
			val t1 = (.5).toFloat
			for (index <- List.range(0,ch.chromosome.size)){
				if (index > 17) ch.chromosome(index) must beCloseTo(-t1, t1)
				else 			ch.chromosome(index) must beCloseTo(t1, t1)
			}
		}
	}
    "getTopFromPopulation" should {
        "have the correct chromsomes" >> {
            val numberOfBest = 2
            val values = List((0,1), (1,2), (2,3), (3,4), (4,5),(5,6)) 
            val pop = Population.apply(10)(35)
            getTopFromPopulation(numberOfBest, values, pop) === List(pop.population(4), pop.population(5))
        }
    }
    "indexesOfBest" should {
        "get the best values from a List of tuples by the second index" >> {
            val numberOfBest = 5
            val values = List((0,6), (1,2), (2,3), (3,4), (4,5),(5,6))
            indexesOfBest(numberOfBest, values) === List(2,3,4,5,0)
        }
        "return all indexes if numberOfBest is greater than List size" >> {
            val values = List((0,1), (1,2), (2,3), (3,4))
            val numberOfBest = 5
            indexesOfBest(numberOfBest, values) === List(0,1,2,3)
        }
        "return all indexes if numberOfBest is equal to List size" >> {
            val values = List((0,1), (1,2), (2,3), (3,4),(4,5))
            val numberOfBest = 5
            indexesOfBest(numberOfBest, values) === List(0,1,2,3,4)
        }
    }
    "addChromosomeToPopulation" should {
        "return a new population with chromsome added" >> {
        	val ch = Chromosome(35)
        	population.size === 1
        	val newPop = addChromosomeToPopulation(ch, population)
        	newPop.size === 2
        }
    }
   "getChromosomeFromPopulation" should {
    	"get the correct chromosome from the population" >> {
    		val ch = Chromosome.basicChromosome(35)
    		val newPop = addChromosomeToPopulation(ch, population)
    		val chromo = getChromosomeFromPopulation(newPop,0)
    		chromo === ch
    	}
    }
    "firstGenerationPopulation" should {
    	"create population of size 10 and chromosomes of size 20" >> {
    		val pop =  firstGenerationPopulation(10, 20)
    		pop.size === 10
    		for (chromo <- pop.population) chromo.size === 20
    	}
       	"create population of size 100 and chromosomes of size 35" >> {
    		val pop =  firstGenerationPopulation(100, 35)
    		pop.size === 100
    		for (chromo <- pop.population) chromo.size === 35
    	}
    }
    "breedTwoChromosomes" should {
    	"only have values from either of those two chromosomes" >> {
    		val newCh = breedTwoChromosomes(Chromosome.basicChromosome(35), Chromosome.allZeroChromosome(35))
    		for (value <- newCh.chromosome) value === 0.0 or value === 1.0
    	}
    }
}