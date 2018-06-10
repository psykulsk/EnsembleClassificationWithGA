if (! "genalg" %in% row.names(installed.packages()))
  install.packages("genalg")
if (! "party" %in% row.names(installed.packages()))
  install.packages("party")
if (! "e1071" %in% row.names(installed.packages()))
  install.packages("e1071") # pakiet zawierajacy klasyfikator svm
if (! "caret" %in% row.names(installed.packages()))
  install.packages("caret") # pakiet zawierajacy confusionMatrix
library(e1071)  # pakiet zawierajacy klasyfikator svm
library(genalg)
library(party)
library(caret)

data("iris")

dataSet <- iris

dataSetFormula <- as.formula("Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")

# utworzenie indeksow do rozdzialu danych trenujacych i testowych
# w chwili obecnej dane treningowe to 80% wszystkich danych
index <- sort(sample(1:nrow(dataSet),round(0.8*nrow(dataSet))))

# wybor danych treningowych i testowych
trainingDataSet = dataSet[index,]
testDataSet = dataSet[-index,]

# Zbior do ewaluacji wewnatrz algorytmu ewolucyjnego, bez etykiet
evaluationTestDataSet = trainingDataSet[,1:4]
evaluationTestLabels = as.integer(trainingDataSet$Species)
uniqueLabels = levels(trainingDataSet$Species)

finalEvalDataSet = testDataSet[,1:4]
finalEvalTestLabels = as.integer(testDataSet$Species)

numberOfDecisionTreesInEnsemble = 6
numberOfSVMInEnsemble = 4
numberOfClassifiersInEnsemble = numberOfDecisionTreesInEnsemble + numberOfSVMInEnsemble
numberOfTrainingDataPoints = length(evaluationTestLabels)
# Dla kazdego punktu trenujacego tworzymy liste, w ktorej pola odpowiadaja klasyfikatorom i okreslaja
# czy dany klasyfikator uzywa 
sizeOfChromosome = numberOfClassifiersInEnsemble*numberOfTrainingDataPoints

# "1" identify decision tree classifier and "2" SVM 
# creation of table containing classifier types ID in ensemble
classifierTypes <- c(rep(1, length.out = numberOfDecisionTreesInEnsemble), rep(2, length.out = numberOfSVMInEnsemble))

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
    
    # Jezeli wektor jest jednolity co do klasy (zawiera przyklady jendej klasy) to pomijamy trening
    if(all(diff(as.integer(trainingSubset$Species)) == 0)){
    
      ensemblePredictionResults[i,] <- rep(NA, length.out = numberOfTrainingDataPoints)
    
    }
    else{
      
      # Tworzenie odpowiedniego klasyfikatora na danym podzbiorze i zapis do listy (<<- to operator przypisania do globalnego obiektu)
      if(classifierTypes[i] == 1){
        classifiers[[i]] <<- ctree(dataSetFormula, data=trainingSubset)
      }
      else if(classifierTypes[i] == 2){
        classifiers[[i]] <<- svm(dataSetFormula, data=trainingSubset)
      }
      # Predykcja na zbiorze ewaluacyjnym
      ensemblePredictionResults[i,] <- predict(classifiers[[i]], newdata=evaluationTestDataSet)
    }
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
  
  confMatrixResult = confusionMatrix(as.factor(votingResults), as.factor(evaluationTestLabels))
  
  return(1-confMatrixResult$overall[["Accuracy"]])
}

monitor <- function(obj) {
    minEval = min(obj$evaluations);
    plot(obj, type="hist");
    print(minEval)
}

# Generacja testowego chromosomu
#s = sample(rep(c(1),length.out=numberOfClassifiersInEnsemble*numberOfTrainingDataPoints))
# uruchomienie funkcji na testowym chromosomie
#testresult = evaluate(s)

GAmodel <- rbga.bin(size = sizeOfChromosome, popSize = 100, iters = 5, mutationChance = 0.01, 
                   elitism = T, evalFunc = evaluate, monitorFunc = monitor, verbose = TRUE, showSettings = TRUE)

# Wybranie najlepszego chromosomu
bestChromosome <- GAmodel$population[which.min(GAmodel$evaluations),]
# Budowa klasyfikatorów na jego podstawie
evaluate(bestChromosome)
# Ocena otrzymanej grupy klasyfikatórów zbiorze testowym
ensemblePredictionResults <- matrix(data=NA, nrow=numberOfClassifiersInEnsemble, ncol=length(finalEvalTestLabels))
votingResults <- vector("integer", length = length(finalEvalTestLabels))
for(i in 1:numberOfClassifiersInEnsemble){
  ensemblePredictionResults[i,] <- predict(classifiers[[i]], newdata=finalEvalDataSet)
}
# Głosowanie
for(i in 1:length(finalEvalTestLabels)){
  # sort(table(ensemblePredictionResults[,i]), decreasing = TRUE) zwraca po kolei najczescie wystepujace elementy
  # Bierzemy pierwszy element i uzywajac names() wyciagamy z niego etykiete
  votingResults[i] = as.integer(names(sort(table(ensemblePredictionResults[,i]), decreasing = TRUE))[1])
  #print(sort(table(ensemblePredictionResults[,i])))
}

confMatrixResult = confusionMatrix(as.factor(votingResults), as.factor(finalEvalTestLabels))
print(confMatrixResult)