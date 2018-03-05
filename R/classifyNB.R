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
#'   ## Load data and create document-feature matrices
#'   train_corpus <- quanteda::corpus(x = training_agendas$text)
#'   metadoc(train_corpus, "language") <- "danish"
#'   train_matrix <- quanteda::dfm(train_corpus,
#'                                 stem = TRUE,
#'                                 removeNumbers = FALSE)
#'
#'   test.corpus <- quanteda::corpus(x = test_agendas$text)
#'   metadoc(test_corpus, "language") <- "danish"
#'   test_matrix <- quanteda::dfm(test.corpus,
#'                                stem = TRUE,
#'                                removeNumbers = FALSE)
#'
#'   est <- trainNB(training_agendas$coding, train_matrix)
#'
#'   out <- classifyNB(est, test_matrix, test_agendas)
#' @rdname classifyNB
#' @import Matrix quanteda
#' @export

classifyNB <- function(est, test_matrix, test) {
  ##Error catching and warnings
  if (length(est) != 4) stop('Error in output supplied from trainNB.')
  if (!is.data.frame(test)) stop('Must supply a data frame as test data.')
  if (!quanteda::is.dfm(test_matrix)) stop('Must supply a quanteda dfm as test_matrix.')
  if (NROW(test_matrix) < 2) stop('Too few observations in test_matrix.')
  if (nrow(test_matrix) != nrow(test)) {
    stop('Unequal number of rows in test_matrix and test data.')
  }

  ##Applying classifications
  w_0c <- est[[1]]
  w_jc <- est[[2]]
  nc <- est[[3]]
  pc <- est[[4]]

  ##Trim test dfm to include only features in training data
  test_matrix <- test_matrix[, (quanteda::featnames(test_matrix) %in% colnames(w_jc))]
  ##Trim trained model include only features in test matrix and reorder to match test matrix
  if (length(rownames(w_jc)) == 1) { # convert w_jc back to matrix when number of categories is 2
    rows <- rownames(w_jc)
    w_jc <- w_jc[, quanteda::featnames(test_matrix)]
    w_jc <- t(as.matrix(w_jc))
    rownames(w_jc) <- rows
  } else {
    w_jc <- w_jc[, quanteda::featnames(test_matrix)]
  }
  ##Convert test dfm to appearance indicators instead of counts
  if (any(test_matrix@x > 1)) {
    test_matrix@x[test_matrix@x > 1] <- 1
  }

  ## GETTING POSTERIOR CLASS PROBABILITIES FOR TEST SET
  term.appearance <- test_matrix %*% t(w_jc) #Calculate w_jc * x_i (n x c)
  log_odds <- t( term.appearance ) + w_0c #full log-odds for c-1 non-reference categories
  odds <- cbind(exp(t(log_odds)), rep(1, ncol(log_odds))) #get odds and add column of 1s for reference category (n x c)

  ##Run check for infinite odds
  ##NB: this happens when ref class is extremely (un)likely relative
  ##    to some other class. Odds become infinite (i.e. class 'Inf')
  ##    when they are too large (small) for machine tolerance
  ##    If infinites are found, then shrink all log odds values
  ##    proportionally toward zero. This works because exp(0) = 1
  ##    Thus, both negative and positive log odds shrink towards
  ##    ref class at the same rate.
  if (any(apply(odds, 2, is.finite) == F)) {
    check.if.infinite <- any(apply(odds, 2, is.finite) == F)

    while (check.if.infinite) {
      #Shrink log odds values toward zero (i.e. ref class)
      log_odds <- log_odds * .9

      odds <- cbind(exp(t(log_odds)), rep(1, ncol(log_odds)))

      check.if.infinite <- any(apply(odds, 2, is.finite) == F)
    }
  }

  denominator <- Matrix::rowSums(odds)

  ##Run check for infinite summed odds (rare if individual odds are finite)
  if (any(is.finite(denominator) == F)) {
    check.if.infinite <- any(is.finite(denominator) == F)

    while (check.if.infinite) {
      #Shrink log odds (further) toward zero (i.e. ref class)
      log_odds <- log_odds * .9

      odds <- cbind(exp(t(log_odds)), rep(1, ncol(log_odds)))
     denominator <- Matrix::rowSums(odds)

      check.if.infinite <- any(is.finite(denominator) == F)
    }
  }

  ##Calculate document class posterior probs
  probs <- odds / denominator
  colnames(probs) <- names(nc) #make categories col names of probs matrix

  ## NOTE MATCHES AND PROBABILITY RATIOS
  unconditional_test <- t(probs) > pc # (c x n)
  identify_max <- probs==apply(probs, 1, max) # (n x c)
  ratios_to_unconditional <- t(t(probs) / pc)
  ratios_to_unconditional <- apply(ratios_to_unconditional, 1, scale)
  ratios_to_unconditional <- t(ratios_to_unconditional)

  max_ratios <- ratios_to_unconditional == apply(ratios_to_unconditional, 1, max)
  if (all(Matrix::rowSums(identify_max) == 1)){ #MAXIMUM PROBABILITIES -- WITH ERROR CATCHING
    test$max_posterior <- as.vector(t(probs))[as.vector(t(identify_max))]
  } else {
    test$max_posterior <- NA
    row.picker <- Matrix::rowSums(identify_max) == 1
    test$max_posterior[row.picker] <- as.vector(t(probs[row.picker, ]))[as.vector(t(identify_max[row.picker, ]))]
    for (j in 1:length(test$max_posterior[row.picker == FALSE])){
      test$max_posterior[row.picker == FALSE][j] <- max(probs[row.picker == FALSE,][j, ])
    }
  }
  if (all(Matrix::rowSums(max_ratios) == 1)){ #fill in maximum ratios with error-catching
    test$max_ratios <- as.vector(t(ratios_to_unconditional))[as.vector(t(max_ratios))]
  } else if (sum((apply(max_ratios, 1, sum) == 1) == FALSE) == 1) {
    test$max_ratios[apply(max_ratios, 1, sum)==1] <- as.vector(t(ratios_to_unconditional[apply(max_ratios, 1, sum) == 1]))[as.vector(t(max_ratios[apply(max_ratios, 1, sum) == 1]))]
    test$max_ratios[(apply(max_ratios, 1, sum)==1) == FALSE] <- max(ratios_to_unconditional[(apply(max_ratios, 1, sum) == 1) == FALSE, ])
  } else {
    test$max_ratios <- NA
    row.picker <- apply(max_ratios, 1, sum) == 1
    test$max_ratios[row.picker] <- as.vector(t(ratios_to_unconditional[row.picker, ]))[as.vector(t(max_ratios[row.picker, ]))]
    for (j in 1:length(test$max_ratios[row.picker == FALSE])){
      test$max_ratios[row.picker == FALSE][j] <- mean(ratios_to_unconditional[row.picker == FALSE, ][j, ])
    }
  }
  name_matrix <- matrix(rep(colnames(probs), nrow(probs)), nrow=nrow(probs), ncol=ncol(probs), byrow = TRUE)
  if (all(Matrix::rowSums(identify_max) == 1)){ #fill in max prob category names with error-catching
    test$max_match <- as.vector(t(name_matrix))[as.vector(t(as.matrix(identify_max)))]
  } else {
    test$max_match <- NA
    row.picker <- apply(identify_max, 1, sum) == 1
    test$max_match[row.picker] <- as.vector(t(name_matrix[row.picker, ]))[as.vector(t(identify_max[row.picker, ]))]
    for (j in 1:length(test$max_match[row.picker == FALSE])){
      boolian.vector <- max_ratios[row.picker == FALSE, ][j, ]
      class.probs <- pc[colnames(probs)[boolian.vector]]
      test$max_match[row.picker == FALSE][j] <- names(class.probs)[which(class.probs == min(class.probs))]
    }
  }
  if (all(Matrix::rowSums(max_ratios) == 1)){ #fill in max ratio category names with error-catching
    test$ratio_match <- as.vector(t(name_matrix))[as.vector(t(as.matrix(max_ratios)))]
  } else if (sum((Matrix::rowSums(max_ratios) == 1) == FALSE) == 1) {
    test$ratio_match <- NA
    row.picker <- apply(max_ratios, 1, sum)==1
    test$ratio_match[row.picker] <- as.vector(t(name_matrix[row.picker, ]))[as.vector(t(max_ratios[row.picker, ]))]
    boolian.vector <- max_ratios[(row.picker) == FALSE, ]
    class.probs <- pc[colnames(probs)[boolian.vector]]
    test$ratio_match[row.picker == FALSE] <- names(class.probs)[which(class.probs == min(class.probs))]
  } else {
  test$ratio_match <- NA
    row.picker <- Matrix::rowSums(max_ratios) == 1
    test$ratio_match[row.picker] <- as.vector(t(name_matrix[row.picker, ]))[as.vector(t(max_ratios[row.picker, ]))]
    for (j in 1:length(test$ratio_match[row.picker == FALSE])){
      boolian.vector <- max_ratios[row.picker == FALSE, ][j, ]
      class.probs <- pc[colnames(probs)[boolian.vector]]
      if(all((sum(class.probs) / length(class.probs)) == class.probs)){
        test$ratio_match[row.picker == FALSE][j] <- sample(names(class.probs), 1)
      } else {
        test$ratio_match[row.picker == FALSE][j] <- names(class.probs)[which(class.probs == min(class.probs))]
      }
    }
  }
  test$ratio_match <- as.numeric(test$ratio_match)
  test$max_match <- as.numeric(test$max_match)
  return(test)
}
