#' Train Naive Bayes
#'
#' Trains multiclass Naive Bayes classifier
#' @param coding Numeric vector of training document codings
#' @param train_matrix A \code{quanteda} document-feature matrix with the number of rows equal to the length of \code{coding}
#' @param smoothing Type of Laplacian smoothing for term priors. See 'Details'.
#' @param alpha Smoothing hyperparameter for 'parameterized' smoothing
#' @param beta Smoothing hyperparameter for 'parameterized' smoothing
#'
#' @return A list with the elements
#' \item{w_0c}{Constant portion of NB classification probabilities.}
#' \item{w_jc}{Portion of NB classification probabilities that varies with test document word appearances.}
#' \item{nc}{Frequency of each category in training documents (named numeric vector)}
#' \item{theta_c}{Unsmoothed prior class probabilities (named numeric vector)}
#'
#' @details
#' Smoothing method defaults to 'normalized' using the system advocated by
#' Frank and Bouckaert (2006) for per-class word vector normalization.
#'
#' Using 'simple' will employ a simple version of Laplacian smoothing described
#' in Metsis et al. (2006). Prior probability of term appearance, given a class,
#' is just frequency of term in class plus 1 over count of documents in class
#' plus 2.
#'
#' Using 'parameterized' will use a version of smoothing mentioned in O'Neil &
#' Schutt (2013) for multiclass Naive bayes. Prior prob. of term appearance,
#' given a class, is frequency of term in class plus alpha minus 1 over count
#' of documents in class plus alpha plus beta minus 2.
#'
#' Using 'none' is inadvisable. In this case, prior prob. of term appearance,
#' given a class, is frequency of term in class over count of documents in class.
#' This will likely generate zero priors, which is a problem.
#'
#' @author Matt W. Loftis
#'
#' @references
#' Frank, E. and Bouckaert, R.R. (2006) Naive Bayes for Text Classification with
#' Unbalanced Classes. s, Knowledge Discovery in Databases: PKDD, 503-510.
#'
#' Metsis, V. Androutsopoulos, I. and Paliouras, G. (2006) Spam Filtering with
#' Naive Bayes -- Which Naive Bayes? CEAS 2006 - Third Conference on Email and
#' Anti-Spam, July 27-28, 2006, Mountain View, California USA.
#'
#' O'Neil, C. and Schutt, R. (2013) Doing Data Science: Straight Talk from the
#' Frontline. O'Reilly.
#'
#' @examples
#' ## Load data and create document-feature matrices
#' train_corpus <- quanteda::corpus(x = training_agendas$text)
#' train_matrix <- quanteda::dfm(train_corpus,
#'                     language = "danish",
#'                     stem = TRUE,
#'                     removeNumbers = FALSE)
#'
#' test_corpus <- quanteda::corpus(x = test_agendas$text)
#' test_matrix <- quanteda::dfm(test_corpus,
#'                    language = "danish",
#'                    stem = TRUE,
#'                    removeNumbers = FALSE)
#'
#' ## Convert matrix of frequencies to matrix of indicators
#' train_matrix@x[train_matrix@x > 1] <- 1
#' test_matrix@x[test_matrix@x > 1] <- 1
#'
#' ## Dropping training features not in the test set
#' train_matrix <- train_matrix[, (colnames(train_matrix) %in% colnames(test_matrix))]
#'
#' est <- trainNB(training_agendas$coding, train_matrix)
#'
#' @rdname trainNB
#' @import Matrix quanteda
#' @export


trainNB <- function(coding, train_matrix, smoothing = c("normalized",
                "simple", "parameterized", "none"), alpha = 2, beta = 10,
                custom.class.priors = NULL) { ##TRAINING CLASSIFIER
  ##Error catching and warnings
  if (length(coding) != nrow(train_matrix)) {
    stop('Number of codings does not equal number of documents in training document-feature matrix')
  }
  if (any(is.na(coding))){
    warning('Missing values present in coding. Removed observations with missing coding.')
    train_matrix <- train_matrix[!is.na(coding), ]
    coding <- coding[!is.na(coding)]
  }
  if (!quanteda::is.dfm(train_matrix)) stop('Must supply a quanteda dfm as train_matrix.')
  if (!is.numeric(coding)) stop('Coding is not numeric. agendacodeR currently requires numeric codings.')

  ##Preliminary items
  c <- length(unique(coding)) #total categories (1 x 1)
  nc <- as.vector(table(coding)) #number of training obs per category (c x 1)
  names(nc) <- names(table(coding)) #naming nc vector with category names
  
  ##Calculate or accept simple class priors
  if (!is.null(custom.class.priors)) {
    if(!is.vector(custom.class.priors)) stop('Custom class priors must be a vector')
    if(!is.numeric(custom.class.priors)) stop('Custom class priors must be numeric')
    if(length(custom.class.priors) != length(nc)) stop('Incorrect number of custom class priors')
    if(any(names(custom.class.priors) != names(nc))) stop('Custom class priors do not match training codings')
    theta_c <- custom.class.priors
  } else {
    theta_c <- nc / nrow(train_matrix) #simple prior probs of categories (c x 1)
  }
  
  ##Reordering these vectors to deal with the reference category problem
  ##If the reference category is the least common category, predictive accuracy is better
  nc <- nc[order(theta_c, decreasing = TRUE)]
  theta_c <- theta_c[order(theta_c, decreasing = TRUE)] #sorting nc and theta_c by theta_c

  ##Calculating priors over terms/categories
  njc <- matrix(NA, nrow = c, ncol = ncol(train_matrix)) #word frequencies by category (c x j)
  rownames(njc) <- names(nc) #apply category names and term names to dimensions
  colnames(njc) <- colnames(train_matrix) #apply category names and term names to dimensions
  for (cat in 1:c){ #loop over categories to count this
    if (length(coding[coding == rownames(njc)[cat]]) > 1) {
      njc[cat,] <- Matrix::colSums(train_matrix[coding == rownames(njc)[cat], ])
    } else {
      njc[cat,] <- as.vector(train_matrix[coding == rownames(njc)[cat], ])
    }
  }

  ##Setting prior prob of words in docs by categories in the training set ################
  #NB: Priors set wrt total terms use in category proposed in Frank and Bouckaert 2006
  if (smoothing == "normalized"){
    j <- ncol(train_matrix) #total number of terms (1 x 1)
    n_notj_c <- matrix(rep(apply(njc, 1, sum), ncol(njc)),
                       ncol = ncol(njc), nrow = (nrow(njc))) - njc #freq of all j in c minus freq of j in c (c x j)
    theta_jc <- (1 + njc) / (j + n_notj_c)
    theta_jc[theta_jc > 1] <- .9999999999999999 #ceiling on odds greater than one
  }

  #NB: Laplacian priors proposed in Metsis et al 2006
  if (smoothing == "simple"){
    theta_jc <- (njc + 1) / (nc + 10)
  }

  #NB: With smoothing hyperparameters proposed in O'Neil & Schutt 2013
  if (smoothing == "parameterized"){
    theta_jc <- (njc + alpha - 1) / (nc + alpha + beta - 2)
  }

  #NB: This is inadvisable. In fact, don't do it. You'll almost certainly get zero priors.
  if (smoothing == "none"){
    theta_jc <- (njc) / (nc)
  }

  ########################################################################################
  ## Main training steps
  foo <- log( (1 - t(theta_jc[-c, ]))/(1 - theta_jc[c, ]) ) #intermediate step to build baseline log odds of each category
  foo <- apply(foo, 2, sum) #intermediate step to build baseline log odds of each category
  w_0c <- foo + log(theta_c[-c] / theta_c[c]) #baseline log odds of categories (invariant to words in vector x_i)
  w_jc <- log((theta_jc[-c, ] * (1 - theta_jc[c, ]) / (theta_jc[c, ] * (1 - theta_jc[-c, ])))) #variable portion of log odds, depending on words in vector x_i

  return( list( w_0c = w_0c, w_jc = w_jc, nc = nc, theta_c = theta_c ) ) #return stuff to make forecasts on new data
}
