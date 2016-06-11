\name{catAccuracy}
\alias{catAccuracy}

\title{Accuracy by categories}
\description{Checks accuracy of classification by category. Provides details by category including: true positive rate, positive predicted value, true frequency in training data, and the top five classes observations from a given category are mistakenly classified into.}
\usage{catAccuracy(true, predicted, latexfile=T, filename="category_accuracy.tex")}
\arguments{
  \item{true}{The vector of true codings}
  \item{predicted}{One vector of predicted codings from \code{classifyNB}}
  \item{latexfile}{Logical indicating whether the user wants a latex table of results output into the current working directory.}
  \item{filename}{String name for the output file. Defaults to \code{category_accuracy.tex}}
}
\value{A dataframe with one row for each class. Columns correspond to within-class measures of: true positive rate, positive predicted value, true frequency in training data, and the top five classes observations from the given category are mistakenly classified into.}
\author{Matt W. Loftis}
\note{Latex table outputs depend on the Latex packages: longtable and xcolor. Include the lines:
\code{\\usepackage\[table\]\{xcolor\}}
\code{\\usepackage\{longtable\}}
in the Latex header
}

\examples{
  out <- classifyNB(est, test_matrix, test)
  acc <- cat.accuracy(true=out$coding, predicted=out$ratio_match, latexfile=T)
}
