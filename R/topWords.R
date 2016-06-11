topWords <- function(est, latexfile=F, filename="topwords.tex"){
  w_jc <- scale(est[[2]])
  top.words <- matrix(nrow=nrow(w_jc), ncol=5)
  for(i in 1:nrow(w_jc)){
    line <- w_jc[rownames(w_jc)[i],]
    line <- line[order(line, decreasing=T)]
    top.words[i,] <- names(line[1:5])
  }
  rownames(top.words) <- rownames(w_jc)
  top.words <- top.words[order(rownames(top.words), decreasing=F),]
  if (latexfile==F){
    return(top.words)
  } else {
    output <- paste("% latex table generated in R by fastMCNB
%", Sys.Date(), "
\\begin{center}
\\begin{footnotesize}
\\begin{longtable}{", paste(rep("l", 1+ncol(top.words)), collapse="|"),"}
\\hline
\\rowcolor{lightgray}
Class &", paste0("\\multicolumn{",ncol(top.words),"}{c}{Top predicting words}"),"\\\\
\\hline
\\rowcolor{lightgray}
 & ", paste0("(",1:ncol(top.words), ")", collapse="&"), "\\\\
\\hline
\\endhead
")
    for(j in 1:nrow(top.words)){
      output <- paste(output,
                      paste(rownames(top.words)[j], "&", paste(top.words[j,], collapse="&"), "\\\\
"))
    }
    output <- paste(output, "\\hline
\\caption{Top words by category \\label{tab:topwords}}
\\end{longtable}
\\end{footnotesize}
\\end{center}
")
    zz <- file(filename,"w")
    cat(output, file=zz)
    close(zz)
    return(top.words)
  }
}
