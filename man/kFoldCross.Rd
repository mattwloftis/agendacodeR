% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kFoldCross.R
\name{kFoldCross}
\alias{kFoldCross}
\title{k-fold cross-validation}
\usage{
kFoldCross(coding, train_matrix, train, k = 10)
}
\arguments{
\item{coding}{The numeric vector of codings}

\item{train_matrix}{A \pkg{quanteda} document-feature matrix with the number of rows equal to the length of \code{coding}}

\item{train}{Data frame from which \code{train_matrix} is derived (must have rows matching \code{train_matrix})}

\item{k}{Scalar argument for the number of folds. Defaults to 10.}
}
\value{
A list of results on the accuracy of each fold's classification.
\item{correct_max}{A list of length k of the proportion of times the class associated with maximum posterior probability was the correct class.}
\item{correct_ratio}{A list of length k of the proportion of times the class associated with maximum ratio of posterior to prior probability was the correct class.}
\item{by.cats}{A list of length k of vectors recording the accuracy by training categories. Accuracy is measured as the proportion of times the class associated with maximum ratio of posterior to prior probability was the correct class, for training observations from the respective class.}
\item{guesses}{A list of length k of data frames. Each data frame contains three columns: the original coding of that fold's training data and the predicted class using both the maximum posterior probability and the maximum ratio of posterior to prior.}
}
\description{
Implements k-fold cross-validation of multiclass NB classifier. Splits training data into k roughly equal parts. For each 'fold,' the classifier trains on the training data not in the fold and checks its accuracy by classifying fold k.
}
\examples{
 ## Load data and create document-feature matrices
 train_corpus <- quanteda::corpus(x = training_agendas$text)
 train_matrix <- quanteda::dfm(train_corpus,
                     language = "danish",
                     stem = TRUE,
                     removeNumbers = FALSE)

## Convert matrix of frequencies to matrix of indicators
train_matrix@x[train_matrix@x > 1] <- 1

set.seed(123)
k <- 3
results <- kFoldCross(training_agendas$coding, train_matrix, training_agendas, k = k)

hist(results[[2]], main = "k-fold Accuracies by Ratio Match")

hist(results[[1]], main = "k-fold Accuracies by Maximum Posterior Match")

##FINDING TOP CATEGORIES FOR PREDICTIVE ACCURACY
cats <- as.data.frame(t(results[[3]][[1]]))
for (j in 2:k) cats <- merge(cats, as.data.frame(t(results[[3]][[j]])), all = TRUE)
accuracies <- apply(cats, 2, function(x) mean(x, na.rm = TRUE))

##Graphical contingency table of all k-fold predictions to data (heatmaps)
codes <- results[[4]][[1]]$true_coding
matches <- results[[4]][[1]]$ratio_match
for (j in 2:k){
  codes <- c(codes, results[[4]][[j]]$true_coding)
  matches <- c(matches, results[[4]][[j]]$ratio_match)
}
tab <- table(codes, matches)
tab <- data.matrix(tab)
tab <- tab[order(as.numeric(rownames(tab))), ]
##NB: GENERATES AN ERROR IF NOT ALL COLUMN NAMES ARE NUMERIC -- STILL WORKS!!
tab <- tab[, order(as.numeric(colnames(tab)))]

heatmap(tab, Rowv = NA, Colv = NA, col = cm.colors(256), scale = "row",
        xlab = "Predicted category", ylab = "Actual category")

}
\author{
Matt W. Loftis
}

