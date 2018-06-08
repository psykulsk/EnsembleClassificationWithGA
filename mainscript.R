if (! "genalg" %in% row.names(installed.packages()))
  install.packages("genalg")
if (! "party" %in% row.names(installed.packages()))
  install.packages("party")
library(genalg)
library(party)

data("iris")
trainingDataSet = iris

plot(rbga.results)


iter = 100
numberOfClassifiersInEnsemble = 10
numberOfTrainingDataPoints = 1000
# Dla kazdego punktu trenujacego tworzymy liste, w ktorej pola odpowiadaja klasyfikatorom i okreslaja
# czy dany klasyfikator uzywa 
sizeOfChromosome = numberOfClassifiersInEnsemble*numberOfTrainingDataPoints
# Generacja testowego chromosomu
s = sample(rep(c(0,1),length.out=numberOfClassifiersInEnsemble*numberOfTrainingDataPoints));

#Stworzenie pustej listy dla klasyfikatorów
classifiers <- vector("list", numberOfClassifiersInEnsemble)
evaluate <- function(chromosome=c()) {
  for(i in 1:numberOfClassifiersInEnsemble){
    startChromosomeIndex = (i-1)*numberOfTrainingDataPoints +1;
    endChromosomeIndex = startChromosomeIndex + numberOfTrainingDataPoints -1;
    # Dla kazdego klasyfikatora wybieramy podzbior danych treningowych za pomoca 0 i 1 w chromosomie
    trainingSubset <- trainingDataSet[chromosome[startChromosomeIndex:endChromosomeIndex],]
    # Tworzenie drzewa na danym podzbiorze i zapis do listy (<<- to operator przypisania do globalnego obiektu)
    classifiers[[i]] <<- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=trainingSubset)
  }
  # Kolejny krok -> ocena klasyfikatorów na zbiorze testowym (trzeb tez zrobic glosowanie)
}
# uruchomienie funkcji na testowym chromosomie
evaluate(s)

#GAmodel <- rbga.bin(size = sizeOfChromosome, popSize = 200, iters = iter, mutationChance = 0.01, 
#                    elitism = T, evalFunc = evaluate)


