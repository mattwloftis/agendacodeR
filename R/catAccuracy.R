catAccuracy <- function(true, predicted, latexfile=F, filename="category_accuracy.tex"){
  tab <- as.matrix(table(true, predicted))
  by.row <- tab/rowSums(tab)
  by.col <- t(t(tab)/colSums(tab))
  frequencies <- rowSums(tab)
  results <- data.frame(true=as.character(rownames(tab)),
                        true.pos.rate=rep(NA, nrow(tab)),
                        pos.pred.val=rep(NA, nrow(tab)),
                        frequency=rep(NA, nrow(tab)),
                        first=rep(NA, nrow(tab)),
                        second=rep(NA, nrow(tab)),
                        third=rep(NA, nrow(tab)),
                        fourth=rep(NA, nrow(tab)),
                        fifth=rep(NA, nrow(tab)))
  for(j in 1:nrow(tab)){
    line <- by.row[rownames(tab)[j],]
    if (rownames(tab)[j] %in% names(line)){
      results[j,2] <- signif(line[names(line)==rownames(tab)[j]], digits=3)
      top <- line[names(line)!=rownames(tab)[j]]
      top <- top[order(top, decreasing=T)]
      top <- top[top>0]
      top <- top[1:5]
      names(top)[is.na(names(top))] <- "n/a"
      results[j,5:ncol(results)] <- names(top)
    } else {
      results[j,2] <- 0
      results[j,3:ncol(results)] <- rep("n/a", (ncol(results)-2))
    }
    if (rownames(tab)[j] %in% colnames(tab)){
      column <- by.col[,rownames(tab)[j]]
      results[j,3] <- signif(column[names(column)==rownames(tab)[j]], digits=3)
    } else {
      results[j,3] <- "n/a"
    }
    results[j,4] <- frequencies[rownames(tab)[j]]
  }
  results <- results[order(results[,2], decreasing=T),]
  # results <- rbind(results[results[,2]!="n/a",],results[results[,2]=="n/a",])
  rownames(results) <- NULL
  results$true <- as.character(results$true)
  if(latexfile==F){
    return(results)
  } else {
    output <- paste("% latex table generated in R by fastMCNB
%", Sys.Date(), "
\\begin{center}
\\begin{footnotesize}
\\begin{longtable}{", paste(rep("c", ncol(results)), collapse="|"),"}
\\hline
\\rowcolor{lightgray}
Class & True Positive & Positive & True &", paste0("\\multicolumn{",5,"}{c}{Top Mistaken Classes}"),"\\\\
\\rowcolor{lightgray}
 & Rate & Predictive Value & Frequency &", paste0("(",1:5, ")", collapse="&"), "\\\\
\\hline
\\endhead
")
    for(j in 1:nrow(results)){
      output <- paste(output,
                      paste(results[j,], collapse="&"), "\\\\
")
    }
    output <- paste(output, "\\hline
\\caption{Accuracy by category \\label{tab:accuracy}}
\\end{longtable}
\\end{footnotesize}
\\end{center}
")
    zz <- file(filename,"w")
    cat(output, file=zz)
    close(zz)
    return(results)
  }
}


