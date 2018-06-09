if (! "genalg" %in% row.names(installed.packages()))
  install.packages("genalg")
if (! "party" %in% row.names(installed.packages()))
  install.packages("party")
library(genalg)
library(party)

data("iris")
trainingDataSet = iris
# Zbior do ewaluacji wewnatrz algorytmu ewolucyjnego, bez etykiet
evaluationTestDataSet = iris[,1:4]
evaluationTestLabels = as.integer(iris$Species)
uniqueLabels = levels(iris$Species)

numberOfClassifiersInEnsemble = 10
numberOfTrainingDataPoints = length(evaluationTestLabels)
# Dla kazdego punktu trenujacego tworzymy liste, w ktorej pola odpowiadaja klasyfikatorom i okreslaja
# czy dany klasyfikator uzywa 
sizeOfChromosome = numberOfClassifiersInEnsemble*numberOfTrainingDataPoints


#Stworzenie pustej listy dla klasyfikatorów
classifiers <- vector("list", numberOfClassifiersInEnsemble)
evaluate <- function(chromosome=c()) {
  ensemblePredictionResults <- matrix(data=NA, nrow=numberOfClassifiersInEnsemble, ncol=length(evaluationTestLabels))
  votingResults <- vector("integer", length = length(evaluationTestLabels))
  for(i in 1:numberOfClassifiersInEnsemble){
    startChromosomeIndex = (i-1)*numberOfTrainingDataPoints +1;
    endChromosomeIndex = startChromosomeIndex + numberOfTrainingDataPoints -1;
    # Dla kazdego klasyfikatora wybieramy podzbior danych treningowych za pomoca 0 i 1 w chromosomie
    trainingSubset <- trainingDataSet[which(chromosome[startChromosomeIndex:endChromosomeIndex]==1),]
    # Tworzenie drzewa na danym podzbiorze i zapis do listy (<<- to operator przypisania do globalnego obiektu)
    classifiers[[i]] <<- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=trainingSubset)
    # Predykcja na zbiorze ewaluacyjnym
    ensemblePredictionResults[i,] <- predict(classifiers[[i]], newdata=evaluationTestDataSet)
  }
  # Głosowanie
  for(i in 1:length(evaluationTestLabels)){
    # sort(table(ensemblePredictionResults[,i]), decreasing = TRUE) zwraca po kolei najczescie wystepujace elementy
    # Bierzemy pierwszy element i uzywajac names() wyciagamy z niego etykiete
    votingResults[i] = as.integer(names(sort(table(ensemblePredictionResults[,i]), decreasing = TRUE))[1])
    #print(sort(table(ensemblePredictionResults[,i])))
  }
  
  nrOfCorrectPredictions = sum(votingResults == evaluationTestLabels)
  nrOfWrongPredictions = length(evaluationTestLabels) - nrOfCorrectPredictions
  
  return(nrOfWrongPredictions/length(evaluationTestLabels))
}

monitor <- function(obj) {
    minEval = min(obj$evaluations);
    plot(obj, type="hist");
    print(minEval)
}

# Generacja testowego chromosomu
s = sample(rep(c(1),length.out=numberOfClassifiersInEnsemble*numberOfTrainingDataPoints))
# uruchomienie funkcji na testowym chromosomie
#testresult = evaluate(s)

GAmodel <- rbga.bin(size = sizeOfChromosome, popSize = 100, iters = 50, mutationChance = 0.01, 
                   elitism = T, evalFunc = evaluate, monitorFunc = monitor, verbose = TRUE)


