#' Find top words by category
#'
#' Identifies training words (features) most associated with each class in the training data.
#' @param est The output object from \code{trainNB}
#' @param latexfile Logical indicating whether the user wants a latex table of results output into the current working directory.
#' @param filename String name for the output file. Defaults to \code{topwords.tex}
#'
#' @return A dataframe with one row for each class. Columns correspond to class name and the top five words associated with the respective class.
#'
#' @author Matt W. Loftis
#' @examples
#' ## Load data and create document-feature matrices
#'   train_corpus <- quanteda::corpus(x = training_agendas$text)
#'   metadoc(train_corpus, "language") <- "danish"
#'   train_matrix <- quanteda::dfm(train_corpus,
#'                                 stem = TRUE,
#'                                 removeNumbers = FALSE)
#'
#' est <- trainNB(training_agendas$coding, train_matrix)
#' words <- topWords(est)
#'
#' @note Latex table outputs depend on the Latex packages: longtable and xcolor. Include the lines:
#' \code{\\usepackage\[table\]\{xcolor\}}
#' \code{\\usepackage\{longtable\}}
#' in the Latex header
#'
#' @rdname topWords
#' @import Matrix quanteda
#' @export

topWords <- function(est, latexfile = FALSE, filename = "topwords.tex"){
  ##Error catching and warnings
  if (length(est) != 4) stop('Error in output supplied from trainNB.')

  ##Analyzing top words
  w_jc <- scale(est[[2]])
  top.words <- matrix(nrow = nrow(w_jc), ncol = 5)
  for (i in 1:nrow(w_jc)) {
    line <- w_jc[rownames(w_jc)[i], ]
    line <- line[order(line, decreasing = TRUE)]
    top.words[i, ] <- names(line[1:5])
  }
  rownames(top.words) <- rownames(w_jc)
  top.words <- top.words[order(rownames(top.words), decreasing = FALSE), ]
  if (latexfile == FALSE) {
    return(top.words)
  } else {
    output <- paste("% latex table generated in R by agendacodeR -- github.com/mattwloftis/agendacodeR
%", Sys.Date(), "
\\begin{center}
\\begin{footnotesize}
\\begin{longtable}{", paste(rep("l", 1 + ncol(top.words)), collapse = "|"), "}
\\hline
\\rowcolor{lightgray}
Class &", paste0("\\multicolumn{", ncol(top.words), "}{c}{Top predicting words}"), "\\\\
\\hline
\\rowcolor{lightgray}
 & ", paste0("(", 1:ncol(top.words), ")", collapse = "&"), "\\\\
\\hline
\\endhead
")
    for (j in 1:nrow(top.words)) {
      output <- paste(output,
                      paste(rownames(top.words)[j], "&", paste(top.words[j, ], collapse = "&"), "\\\\
"))
    }
    output <- paste(output, "\\hline
\\caption{Top words by category \\label{tab:topwords}}
\\end{longtable}
\\end{footnotesize}
\\end{center}
")
    zz <- file(filename, "w")
    cat(output, file = zz)
    close(zz)
    return(top.words)
  }
}
