#' k-fold cross-validation
#'
#' Implements k-fold cross-validation of multiclass NB classifier. Splits training data into k roughly equal parts. For each 'fold,' the classifier trains on the training data not in the fold and checks its accuracy by classifying fold k.
#' @param coding The numeric vector of codings
#' @param train_matrix A \pkg{quanteda} document-feature matrix with the number of rows equal to the length of \code{coding}
#' @param train Data frame from which \code{train_matrix} is derived (must have rows matching \code{train_matrix})
#' @param k Scalar argument for the number of folds. Defaults to 10.
#'
#' @return A list of results on the accuracy of each fold's classification.
#' \item{correct_max}{A list of length k of the proportion of times the class associated with maximum posterior probability was the correct class.}
#' \item{correct_ratio}{A list of length k of the proportion of times the class associated with maximum ratio of posterior to prior probability was the correct class.}
#' \item{by.cats}{A list of length k of vectors recording the accuracy by training categories. Accuracy is measured as the proportion of times the class associated with maximum ratio of posterior to prior probability was the correct class, for training observations from the respective class.}
#' \item{guesses}{A list of length k of data frames. Each data frame contains three columns: the original coding of that fold's training data and the predicted class using both the maximum posterior probability and the maximum ratio of posterior to prior.}
#'
#' @author Matt W. Loftis
#' @import Matrix quanteda
#' @examples
#'  ## Load data and create document-feature matrices
#'  train_corpus <- quanteda::corpus(x=training_agendas$text)
#'  train_matrix <- quanteda::dfm(train_corpus,
#'                      language="danish",
#'                      stem=TRUE,
#'                      removeNumbers=FALSE)
#'
#' ## Convert matrix of frequencies to matrix of indicators
#' train_matrix@x[train_matrix@x>1] <- 1
#'
#' set.seed(123)
#' k <- 3
#' results <- kFoldCross(training_agendas$coding, train_matrix, training_agendas, k=k)
#'
#' hist(results[[2]], main="k-fold Accuracies by Ratio Match")
#'
#' hist(results[[1]], main="k-fold Accuracies by Maximum Posterior Match")
#'
#' ##FINDING TOP CATEGORIES FOR PREDICTIVE ACCURACY
#' cats <- as.data.frame(t(results[[3]][[1]]))
#' for (j in 2:k) cats <- merge(cats, as.data.frame(t(results[[3]][[j]])), all=TRUE)
#' accuracies <- apply(cats,2,function(x) mean(x, na.rm=TRUE))
#'
#' ##Graphical contingency table of all k-fold predictions to data (heatmaps)
#' codes <- results[[4]][[1]]$true_coding
#' matches <- results[[4]][[1]]$ratio_match
#' for (j in 2:k){
#'   codes <- c(codes, results[[4]][[j]]$true_coding)
#'   matches <- c(matches, results[[4]][[j]]$ratio_match)
#' }
#' tab <- table(codes, matches)
#' tab <- data.matrix(tab)
#' tab <- tab[order(as.numeric(rownames(tab))),]
#' ##NB: GENERATES AN ERROR IF NOT ALL COLUMN NAMES ARE NUMERIC -- STILL WORKS!!
#' tab <- tab[,order(as.numeric(colnames(tab)))]
#'
#' heatmap(tab, Rowv=NA, Colv=NA, col=cm.colors(256), scale="row",
#'         xlab="Predicted category", ylab="Actual category")
#'
#' @rdname kFoldCross
#' @import Matrix quanteda
#' @export


kFoldCross <- function(coding,train_matrix,train,k=10){
  ##Error catching and warnings
  if(length(coding)!=nrow(train_matrix)) stop('Number of codings does not equal number of documents in training document-feature matrix')
  if(length(coding)!=nrow(train)) stop('Number of codings does not equal number of observations in train data')
  if(nrow(train)!=nrow(train_matrix)) stop('Number of observations in train data does not equal number of documents in training document-feature matrix')
  if(any(is.na(coding))){
    warning('Missing values present in coding. Removed observations with missing coding.')
    coding <- coding[!is.na(coding)]
    train_matrix <- train_matrix[!is.na(coding),]
    train <- train[!is.na(coding),]
  }
  if(!quanteda::is.dfm(train_matrix)) stop('Must supply a quanteda dfm as train_matrix.')
  if(!is.numeric(coding)) stop('Coding is not numeric. agendacodeR currently requires numeric codings.')
  if(!is.data.frame(train)) stop('Must supply a data frame as train data.')

  ##Cross-validating
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
    est <- trainNB(coding[labs!=letters[fold]],train_mat) ##Train NB algorithm - hand-off to function in "run_algorithm.R" file
    ##CLASSIFICATION STEP
    output <- classifyNB(est,test_mat,train[labs==letters[fold],]) ##Hand off to classification function in the "run_algorithm.R" file
    ##CALCULATING ACCURACIES
    output$correct <- as.numeric(coding[labs==letters[fold]]==output$ratio_match) #check answers by ratio match
    a <- split(output, output$ratio_match) #split the data by assigned categories
    by.cats[[fold]] <- unlist( lapply(a, function(x) sum(x$correct)/nrow(x)) ) #record ratio match accuracy by category
    correct_max[fold] <- table(coding[labs==letters[fold]]==output$max_match)["TRUE"]/nrow(output) #record total accuracy of max matches
    correct_ratio[fold] <- table(coding[labs==letters[fold]]==output$ratio_match)["TRUE"]/nrow(output) #record total accuracy of ratio matches
    guesses[[fold]] <- cbind(coding[labs==letters[fold]], output[,c("max_match","ratio_match")])
    colnames(guesses[[fold]]) <- c("true_coding","max_match","ratio_match")
  }
  return(list(correct_max,correct_ratio,by.cats,guesses))
}
