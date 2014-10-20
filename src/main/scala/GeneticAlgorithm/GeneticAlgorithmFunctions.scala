package trent.nfl
import scala.util.Random


trait GeneticAlgorithmScala {

	def getTopFromPopulation(numberOfBest: Int, values: List[(Int, Int)], pop: Population): List[Chromosome] = {
		val indexes = indexesOfBest(numberOfBest, values)
		for (eachIndex <- indexes) yield pop.population(eachIndex)
	}

	def indexesOfBest(numberOfBest: Int, values: List[(Int, Int)]): List[Int] = {
		if (numberOfBest >= values.size) return List.range(0, values.size)
		def go( sizeSoFar: Int, values: List[(Int, Int)], accum: List[Int]): List[Int] = {
			if (sizeSoFar == numberOfBest) return accum

			val maxCh = values.maxBy(_._2)
			val index = values.indexOf(maxCh)
			val newList = values diff List(maxCh)

			println(values + "  " + index)

			go(sizeSoFar +1, newList, maxCh._1 :: accum)
		}
		go(0, values, List())
	}

	def newPopulationFromOld(pop: Population, bestPop: List[Chromosome], popSize: Int): Population = {
		import scala.util.Random

		val bestBreedWithRandomPopulation = for (bestCh <- bestPop) yield {
			val index = Random.nextInt(pop.size)
			val ch = breedTwoChromosomes(bestCh, pop.population(index))
			breedTwoChromosomes(ch, pop.population(index))
		}
		val remainingNeeded = popSize - bestBreedWithRandomPopulation.size 
		val finalPop = for (x <- List.range(0, remainingNeeded)) yield Chromosome.apply(35)
		new Population(finalPop ::: bestBreedWithRandomPopulation)
		
	}

	def breedTwoChromosomes(ch1: Chromosome, ch2: Chromosome): Chromosome = {
		val chList = for (index <- List.range(0, ch1.size))yield {
			if (Random.nextBoolean) ch1.chromosome(index)
			else ch2.chromosome(index)
		}
		new Chromosome(chList)
	}

	def firstGenerationPopulation(popSize: Int, chromoSize: Int) = Population(popSize)(chromoSize)

	def addChromosomeToPopulation(ch: Chromosome, pop: Population): Population = new Population(ch :: pop.population)

	def getChromosomeFromPopulation(pop: Population, index: Int): Chromosome = pop.population(index)

	def resultsToAmountRight(result: List[(Int, Int)]) = {
		val sortedResult = result.sortBy(_._1)
		val slidingResult = sortedResult.sliding(256,256).toList
		val amountRight = slidingResult.map(x => x.foldLeft(0)((b,a) => b + a._2))
		(List.range(0,100), amountRight).zipped.toList
	}
}
