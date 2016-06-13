<<<<<<< HEAD
#' Classify Naive Bayes
#'
#' Classify test data using output from \code{trainNB}
#' @param est Output object from \code{trainNB}
#' @param test_matrix A \pkg{quanteda} document-feature matrix with the same number of rows as \code{test}. Rows must match.
#' @param test The data frame containing the text data from which \code{test_matrix} was created.
#'
#' @return A data frame equal to \code{test} with four added columns:
#' \item{max_posterior}{Maximum posterior probability of any class for this document. This matches the posterior probability of the classification assigned in the \code{max_match} column.}
#' \item{max_ratios}{Maximum ratio of posterior to prior probability of any class for this document. This matches the ratio of the classification assigned in the \code{ratio_match} column.}
#' \item{max_match}{Class associated with maximum posterior probability. Contains predicted class for each observation in \code{test}.}
#' \item{ratio_match}{Class associated with maximum ratio of posterior to prior probability. Contains predicted class for each observation in \code{test}.}
#'
#' @author Matt W. Loftis
#' @examples
#'   library(quanteda)
#'
#'   ## Load data and create document-feature matrices
#'   train_corpus <- corpus(x=training_agendas$text)
#'   train_matrix <- dfm(train_corpus,
#'                       language="danish",
#'                       stem=TRUE,
#'                       removeNumbers=FALSE)
#'
#'   test.corpus <- corpus(x=test_agendas$text)
#'   test_matrix <- dfm(test.corpus,
#'                 language="danish",
#'                 stem=TRUE,
#'                 removeNumbers=FALSE)
#'
#'   ## Convert matrix of frequencies to matrix of indicators
#'   train_matrix@x[train_matrix@x>1] <- 1
#'   test_matrix@x[test_matrix@x>1] <- 1
#'
#'   ## Dropping training features not in the test set
#'   train_matrix <- train_matrix[,(colnames(train_matrix) \%in\% colnames(test_matrix))]
#'
#'
#'   est <- trainNB(training_agendas$coding, train_matrix)
#'
#'   out <- classifyNB(est, test_matrix, test_agendas)
#' @rdname classifyNB
#' @import Matrix quanteda
#' @export

=======
>>>>>>> 069e694d09b3ec714660b30a7155bbbe5ab1e7c4
classifyNB <- function(est, test_matrix, test) {
  w_0c <- est[[1]]
  w_jc <- est[[2]]
  nc <- est[[3]]
  pc <- est[[4]]
<<<<<<< HEAD

  test_matrix <- test_matrix[,(colnames(test_matrix) %in% colnames(w_jc))]
  w_jc <- w_jc[, colnames(test_matrix)]

=======
  
  test_matrix <- test_matrix[,(colnames(test_matrix) %in% colnames(w_jc))]
  w_jc <- w_jc[, colnames(test_matrix)]
  
>>>>>>> 069e694d09b3ec714660b30a7155bbbe5ab1e7c4
  ## GETTING POSTERIOR CLASS PROBABILITIES FOR TEST SET
  term.appearance <- test_matrix %*% t( w_jc ) #Calculate w_jc * x_i (n x c)
  log_odds <- t( term.appearance ) + w_0c #full log-odds for c-1 non-reference categories
  odds <- cbind(exp( t( log_odds ) ), rep(1, ncol(log_odds))) #get odds and add column of 1s for reference category (n x c)
  denominator <- rowSums(odds)
  probs <- odds/denominator
  colnames(probs) <- names(nc) #make col names of results matrix the categories
<<<<<<< HEAD

=======
  
>>>>>>> 069e694d09b3ec714660b30a7155bbbe5ab1e7c4
  ## NOTE MATCHES AND PROBABILITY RATIOS
  unconditional_test <- t(probs)>pc # (c x n)
  identify_max <- probs==apply(probs,1,max) # (n x c)
  ratios_to_unconditional <- t(t(probs)/pc)
  max_ratios <- ratios_to_unconditional==apply(ratios_to_unconditional,1,max)
  if (all(rowSums(identify_max)==1)){ #MAXIMUM PROBABILITIES -- WITH ERROR CATCHING
    test$max_posterior <- as.vector(t(probs))[as.vector(t(identify_max))]
  } else {
    test$max_posterior <- NA
    row.picker <- rowSums(identify_max)==1
    test$max_posterior[row.picker] <- as.vector(t(probs[row.picker,]))[as.vector(t(identify_max[row.picker,]))]
<<<<<<< HEAD
    for (j in 1:length(test$max_posterior[row.picker==FALSE])){
      test$max_posterior[row.picker==FALSE][j] <- max(probs[row.picker==FALSE,][j,])
=======
    for (j in 1:length(test$max_posterior[row.picker==F])){
      test$max_posterior[row.picker==F][j] <- max(probs[row.picker==F,][j,])
>>>>>>> 069e694d09b3ec714660b30a7155bbbe5ab1e7c4
    }
  }
  if (all(rowSums(max_ratios)==1)){ #fill in maximum ratios with error-catching
    test$max_ratios <- as.vector(t(ratios_to_unconditional))[as.vector(t(max_ratios))]
<<<<<<< HEAD
  } else if (sum((apply(max_ratios,1,sum)==1)==FALSE)==1) {
    test$max_ratios[apply(max_ratios,1,sum)==1] <- as.vector(t(ratios_to_unconditional[apply(max_ratios,1,sum)==1]))[as.vector(t(max_ratios[apply(max_ratios,1,sum)==1]))]
    test$max_ratios[(apply(max_ratios,1,sum)==1)==FALSE] <- max(ratios_to_unconditional[(apply(max_ratios,1,sum)==1)==FALSE,])
=======
  } else if (sum((apply(max_ratios,1,sum)==1)==F)==1) {
    test$max_ratios[apply(max_ratios,1,sum)==1] <- as.vector(t(ratios_to_unconditional[apply(max_ratios,1,sum)==1]))[as.vector(t(max_ratios[apply(max_ratios,1,sum)==1]))]
    test$max_ratios[(apply(max_ratios,1,sum)==1)==F] <- max(ratios_to_unconditional[(apply(max_ratios,1,sum)==1)==F,])
>>>>>>> 069e694d09b3ec714660b30a7155bbbe5ab1e7c4
  } else {
    test$max_ratios <- NA
    row.picker <- apply(max_ratios,1,sum)==1
    test$max_ratios[row.picker] <- as.vector(t(ratios_to_unconditional[row.picker,]))[as.vector(t(max_ratios[row.picker,]))]
<<<<<<< HEAD
    for (j in 1:length(test$max_ratios[row.picker==FALSE])){
      test$max_ratios[row.picker==FALSE][j] <- mean(ratios_to_unconditional[row.picker==FALSE,][j,])
=======
    for (j in 1:length(test$max_ratios[row.picker==F])){
      test$max_ratios[row.picker==F][j] <- mean(ratios_to_unconditional[row.picker==F,][j,])
>>>>>>> 069e694d09b3ec714660b30a7155bbbe5ab1e7c4
    }
  }
  name_matrix <- matrix(rep(colnames(probs),nrow(probs)),nrow=nrow(probs),ncol=ncol(probs),byrow=T)
  if (all(rowSums(identify_max)==1)){ #fill in max prob category names with error-catching
    test$max_match <- as.vector(t(name_matrix))[as.vector(t(as.matrix(identify_max)))]
  } else {
    test$max_match <- NA
    row.picker <- apply(identify_max,1,sum)==1
    test$max_match[row.picker] <- as.vector(t(name_matrix[row.picker,]))[as.vector(t(identify_max[row.picker,]))]
<<<<<<< HEAD
    for (j in 1:length(test$max_match[row.picker==FALSE])){
      boolian.vector <- max_ratios[row.picker==FALSE,][j,]
      # test$max_match[row.picker==FALSE][j] <- paste(colnames(probs)[boolian.vector],collapse="; ")
      class.probs <- pc[colnames(probs)[boolian.vector]]
      test$max_match[row.picker==FALSE][j] <- names(class.probs)[which(class.probs==min(class.probs))]
=======
    for (j in 1:length(test$max_match[row.picker==F])){
      boolian.vector <- max_ratios[row.picker==F,][j,]
      # test$max_match[row.picker==F][j] <- paste(colnames(probs)[boolian.vector],collapse="; ")
      class.probs <- pc[colnames(probs)[boolian.vector]]
      test$max_match[row.picker==F][j] <- names(class.probs)[which(class.probs==min(class.probs))]
>>>>>>> 069e694d09b3ec714660b30a7155bbbe5ab1e7c4
    }
  }
  if (all(rowSums(max_ratios)==1)){ #fill in max ratio category names with error-catching
    test$ratio_match <- as.vector(t(name_matrix))[as.vector(t(as.matrix(max_ratios)))]
<<<<<<< HEAD
  } else if (sum((rowSums(max_ratios)==1)==FALSE)==1) {
    test$ratio_match <- NA
    row.picker <- apply(max_ratios,1,sum)==1
    test$ratio_match[row.picker] <- as.vector(t(name_matrix[row.picker,]))[as.vector(t(max_ratios[row.picker,]))]
    boolian.vector <- max_ratios[(row.picker)==FALSE,]
    class.probs <- pc[colnames(probs)[boolian.vector]]
    test$ratio_match[row.picker==FALSE] <- names(class.probs)[which(class.probs==min(class.probs))]
=======
  } else if (sum((rowSums(max_ratios)==1)==F)==1) {
    test$ratio_match <- NA
    row.picker <- apply(max_ratios,1,sum)==1
    test$ratio_match[row.picker] <- as.vector(t(name_matrix[row.picker,]))[as.vector(t(max_ratios[row.picker,]))]
    boolian.vector <- max_ratios[(row.picker)==F,]
    class.probs <- pc[colnames(probs)[boolian.vector]]
    test$ratio_match[row.picker==F] <- names(class.probs)[which(class.probs==min(class.probs))]
>>>>>>> 069e694d09b3ec714660b30a7155bbbe5ab1e7c4
  } else {
  test$ratio_match <- NA
    row.picker <- rowSums(max_ratios)==1
    test$ratio_match[row.picker] <- as.vector(t(name_matrix[row.picker,]))[as.vector(t(max_ratios[row.picker,]))]
<<<<<<< HEAD
    for (j in 1:length(test$ratio_match[row.picker==FALSE])){
      boolian.vector <- max_ratios[row.picker==FALSE,][j,]
      class.probs <- pc[colnames(probs)[boolian.vector]]
      if(all((sum(class.probs)/length(class.probs))==class.probs)){
        test$ratio_match[row.picker==FALSE][j] <- sample(names(class.probs), 1)
      } else {
        test$ratio_match[row.picker==FALSE][j] <- names(class.probs)[which(class.probs==min(class.probs))]
=======
    for (j in 1:length(test$ratio_match[row.picker==F])){
      boolian.vector <- max_ratios[row.picker==F,][j,]
      class.probs <- pc[colnames(probs)[boolian.vector]]
      if(all((sum(class.probs)/length(class.probs))==class.probs)){
        test$ratio_match[row.picker==F][j] <- sample(names(class.probs), 1)
      } else {
        test$ratio_match[row.picker==F][j] <- names(class.probs)[which(class.probs==min(class.probs))]
>>>>>>> 069e694d09b3ec714660b30a7155bbbe5ab1e7c4
      }
    }
  }
  return(test)
}
