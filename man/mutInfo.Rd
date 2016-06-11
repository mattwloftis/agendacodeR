\name{mutInfo}
\alias{mutInfo}
\title{Calculate Mutual Information}
\description{Calculate mutual information between training classes and training features}
\usage{mutInfo(coding, train_matrix)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coding}{The vector of codings}
  \item{train_matrix}{A \code{quanteda} document-feature matrix with the number of rows equal to the length of \code{coding}.}
}
\value{A numeric vector the same length as \code{features(train_matrix)}}
\references{McCallum, Andrew \& Kamal Nigam. 1998. A Comparison of Event Models for Naive Bayes Text Classification. In \textit{Learning for Text Categorization: Papers from the AAAI Workshop, AAAI Press.}}
\author{Matt W. Loftis}
\examples{
  library(quanteda)
  
  ## Load data and create document-feature matrices
  train.corpus <- corpus(x=training_agendas$text)
  train_matrix <- dfm(train_corpus, 
                      language="danish", 
                      stem=T, 
                      removeNumbers=F)
  
  ## Mutual information algorithm for feature selection
  mut.info <- mutInfo(train$coding, train_matrix)
  cutoff <- quantile(avg.mut.info, .8) #Set cutoff quantile for mutual information
  train_matrix <- train_matrix[,avg.mut.info>cutoff] #Pare down training set
}