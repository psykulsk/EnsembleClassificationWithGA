if (! "genalg" %in% row.names(installed.packages()))
  install.packages("genalg")
library(genalg)

iter = 100
numberOfClassifiersInEnsemble = 10
numberOfTrainingDataPoints = 1000
# Dla kazdego punktu trenujacego tworzymy liste, w ktorej pola odpowiadaja klasyfikatorom i okreslaja
# czy dany klasyfikator uzywa 
sizeOfChromosome = numberOfClassifiersInEnsemble*numberOfTrainingDataPoints

GAmodel <- rbga.bin(size = , popSize = 200, iters = iter, mutationChance = 0.01, 
                    elitism = T, evalFunc = evalFunc)