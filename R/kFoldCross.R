kFoldCross <- function(coding,train_matrix,k=10){
  size <- ceiling(length(coding)/k) #getting approximate size of folds
  labs <- rep(letters[1:k],size) #generating letters for labeling folds
  labs <- labs[1:length(coding)] #reducing label vector to size of training set
  labs <- sample(labs, length(labs), replace=F) #randomizing labels to assign folds
  correct_max <- rep(NA,k) #vector of max probability accuracies
  correct_ratio <- rep(NA,k) #vector of ratio probability accuracies
  guesses <- list() #tables of actual vs. predicted codings
  by.cats <- list() #list of accuracies by category per fold
  pb <- txtProgressBar(max=k, style=3, width=90)
  for (fold in 1:k){ #Looping over k folds
    setTxtProgressBar(pb,fold)
    train_mat <- train_matrix[labs!=letters[fold],] #pulling out training set for fold
    test_mat <- train_matrix[labs==letters[fold],] #pulling out test set for fold
    train_mat <- train_mat[,colSums(train_mat)>0] #dropping terms in train only appearing in test
    test_mat <- test_mat[,colSums(test_mat)>0] #dropping terms in test only appearing in train
    
    ##TRAINING STEP
    est <- train.NB(coding[labs!=letters[fold]],train_mat) ##Train NB algorithm - hand-off to function in "run_algorithm.R" file
    ##CLASSIFICATION STEP
    output <- classify.NB(est,test_mat,train[labs==letters[fold],]) ##Hand off to classification function in the "run_algorithm.R" file
    ##CALCULATING ACCURACIES
    output$correct <- as.numeric(output$coding==output$ratio_match) #check answers by ratio match
    a <- split(output, output$ratio_match) #split the data by assigned categories
    by.cats[[fold]] <- unlist( lapply(a, function(x) sum(x$correct)/nrow(x)) ) #record ratio match accuracy by category
    correct_max[fold] <- table(output$coding==output$max_match)["TRUE"]/nrow(output) #record total accuracy of max matches
    correct_ratio[fold] <- table(output$coding==output$ratio_match)["TRUE"]/nrow(output) #record total accuracy of ratio matches
    guesses[[fold]] <- output[,c("coding","max_match","ratio_match")]
  }
  return(list(correct_max,correct_ratio,by.cats,guesses))
}
