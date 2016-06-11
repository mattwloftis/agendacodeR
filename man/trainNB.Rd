\name{trainNB}
\alias{trainNB}
\title{Train Naive Bayes}
\usage{
  trainNB(coding, train_matrix)
}
\arguments{
  \item{coding}{The vector of codings}
  \item{train_matrix}{A \code{quanteda} document-feature matrix with the number of rows equal to the length of \code{coding}.}
}
\value{
  A list with the elements
  \item{w_0c}{Constant portion of NB classification probabilities.}
  \item{w_jc}{Portion of NB classification probabilities that varies with test document word appearances.}
  \item{nc}{Frequency of each category in training documents (named numeric vector)}
  \item{theta_c}{Unsmoothed prior class probabilities (named numeric vector)}
}
\description{
  Trains multiclass Naive Bayes classifier.
}
\references{Frank, Eibe & Remco R. Bouckaert. 2006. \textit{Knowledge Discovery in Databases: PKDD 2006: 10th European Conference on Principles and Practice of Knowledge Discovery in Databases Berlin, Germany, September 18-22, 2006 Proceedings.} Berlin, Heidelberg: Springer chapter Naive Bayes for Text Classification with Unbalanced Classes, pp. 503-510.
O'Neil, Cathy & Rachel Schutt. 2013. \textit{Doing Data Science: Straight Talk from the Frontline}. O'Reilly Media.
}
\examples{
  library(quanteda)
  
  ## Load data and create document-feature matrices
  train.corpus <- corpus(x=training_agendas$text)
  train_matrix <- dfm(train_corpus, 
                      language="danish", 
                      stem=T, 
                      removeNumbers=F)

  test.corpus <- corpus(x=test_agendas$text)
  test_matrix <- dfm(test.corpus, 
                      language="danish", 
                      stem=T, 
                      removeNumbers=F)
  
  ## Convert matrix of frequencies to matrix of indicators
  train_matrix@x[train_matrix@x>1] <- 1 
  test_matrix@x[test_matrix@x>1] <- 1
  
  ## Dropping training features not in the test set
  train_matrix <- train_matrix[,(colnames(train_matrix) \%in\% colnames(test_matrix))]

  est <- train.NB(training_agendas$coding, train_matrix)
}
\author{
  Matt W. Loftis
}
