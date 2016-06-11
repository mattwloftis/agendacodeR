\name{topWords}
\alias{topWords}
\title{Find top words by category}
\description{Identifies training words (features) most associated with each class in the training data.}
\usage{topWords(est, latexfile=T, filename="topwords.tex")}
\arguments{
  \item{est}{The output object from \code{trainNB}}
  \item{latexfile}{Logical indicating whether the user wants a latex table of results output into the current working directory.}
  \item{filename}{String name for the output file. Defaults to \code{topwords.tex}}
}
\details{}
\value{A dataframe with one row for each class. Columns correspond to class name and the top five words associated with the respective class.}
\references{}
\author{Matt W. Loftis}
\note{Latex table outputs depend on the Latex packages: longtable and xcolor. Include the lines:
\code{\\usepackage\[table\]\{xcolor\}}
\code{\\usepackage\{longtable\}}
in the Latex header
}
\examples{
  est <- train.NB(training_agendas$coding, train_matrix)
  words <- topWords(est, latexfile=T)
}