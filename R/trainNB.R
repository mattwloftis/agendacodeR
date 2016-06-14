#' Train Naive Bayes
#'
#' Trains multiclass Naive Bayes classifier
#' @param coding The vector of codings
#' @param train_matrix A \code{quanteda} document-feature matrix with the number of rows equal to the length of \code{coding}
#'
#' @return A list with the elements
#' \item{w_0c}{Constant portion of NB classification probabilities.}
#' \item{w_jc}{Portion of NB classification probabilities that varies with test document word appearances.}
#' \item{nc}{Frequency of each category in training documents (named numeric vector)}
#' \item{theta_c}{Unsmoothed prior class probabilities (named numeric vector)}
#'
#' @author Matt W. Loftis
#' @examples
#' ## Load data and create document-feature matrices
#' train_corpus <- quanteda::corpus(x=training_agendas$text)
#' train_matrix <- quanteda::dfm(train_corpus,
#'                     language="danish",
#'                     stem=TRUE,
#'                     removeNumbers=FALSE)
#'
#' test_corpus <- quanteda::corpus(x=test_agendas$text)
#' test_matrix <- quanteda::dfm(test_corpus,
#'                    language="danish",
#'                    stem=TRUE,
#'                    removeNumbers=FALSE)
#'
#' ## Convert matrix of frequencies to matrix of indicators
#' train_matrix@x[train_matrix@x>1] <- 1
#' test_matrix@x[test_matrix@x>1] <- 1
#'
#' ## Dropping training features not in the test set
#' train_matrix <- train_matrix[,(colnames(train_matrix) %in% colnames(test_matrix))]
#'
#' est <- trainNB(training_agendas$coding, train_matrix)
#'
#' @rdname trainNB
#' @import Matrix quanteda
#' @export


trainNB <- function(coding,train_matrix){ ##TRAINING CLASSIFIER
  ##Error catching and warnings
  if(length(coding)!=nrow(train_matrix)) stop('Length of codings does not equal number of documents in training document-feature matrix')
  if(any(is.na(coding))){
    warning('NB: Missing values present in coding. Removing observations with missing coding.')
    coding <- coding[is.na(coding)==FALSE]
    train_matrix <- train_matrix[is.na(coding)==FALSE,]
  }

  ##Preliminary items
  c <- length(unique(coding)) #total categories (1 x 1)
  nc <- as.vector(table(coding)) #number of training obs per category (c x 1)
  names(nc) <- names(table(coding)) #naming nc vector with category names
  theta_c <- nc/nrow(train_matrix) #simple prior probs of categories (c x 1)

  ##Reordering these vectors to deal with the reference category problem
  ##If the reference category is the least common category, predictive accuracy is better
  nc <- nc[order(theta_c, decreasing=T)]
  theta_c <- theta_c[order(theta_c, decreasing=T)] #sorting nc and theta_c by theta_c

  ##Calculating priors over terms/categories
  njc <- matrix(NA, nrow=c, ncol=ncol(train_matrix)) #word frequencies by category (c x j)
  rownames(njc) <- names(nc); colnames(njc) <- colnames(train_matrix) #apply category names and term names to dimensions
  for(cat in 1:c){ #loop over categories to count this
    if(length(coding[coding==rownames(njc)[cat]])>1) {
      njc[cat,] <- colSums(train_matrix[coding==rownames(njc)[cat],])
    } else {
      njc[cat,] <- as.vector(train_matrix[coding==rownames(njc)[cat],])
    }
  }

  ##Setting prior prob of words in docs by categories in the training set ################
  #NB: Priors set wrt total terms use in category proposed in Frank and Bouckaert 2006
  j <- ncol(train_matrix) #total number of terms (1 x 1)
  n_notj_c <- matrix(rep(apply(njc,1,sum),ncol(njc)),
                     ncol=ncol(njc), nrow=(nrow(njc))) - njc #freq of all j in c minus freq of j in c (c x j)
  theta_jc <- (1 + njc)/(j + n_notj_c)
  ########################################################################################
  ## Main training steps
  foo <- log( (1 - t(theta_jc[-c,]))/(1 - theta_jc[c,]) ) #intermediate step to building baseline log odds of each category
  foo <- apply(foo, 2, sum) #intermediate step to building baseline log odds of each category
  w_0c <- foo + log(theta_c[-c]/theta_c[c]) #baseline log odds of each category (invariant to words in vector x_i)
  w_jc <- log( (theta_jc[-c,]*(1-theta_jc[c,]) / (theta_jc[c,]*(1-theta_jc[-c,]))) ) #variable portion of log odds, depending on words in vector x_i

  return( list( w_0c=w_0c, w_jc=w_jc, nc=nc, theta_c=theta_c ) ) #return stuff to make forecasts on new data
}
