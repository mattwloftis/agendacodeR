classifyNB <- function(est, test_matrix, test) {
  w_0c <- est[[1]]
  w_jc <- est[[2]]
  nc <- est[[3]]
  pc <- est[[4]]
  
  test_matrix <- test_matrix[,(colnames(test_matrix) %in% colnames(w_jc))]
  w_jc <- w_jc[, colnames(test_matrix)]
  
  ## GETTING POSTERIOR CLASS PROBABILITIES FOR TEST SET
  term.appearance <- test_matrix %*% t( w_jc ) #Calculate w_jc * x_i (n x c)
  log_odds <- t( term.appearance ) + w_0c #full log-odds for c-1 non-reference categories
  odds <- cbind(exp( t( log_odds ) ), rep(1, ncol(log_odds))) #get odds and add column of 1s for reference category (n x c)
  denominator <- rowSums(odds)
  probs <- odds/denominator
  colnames(probs) <- names(nc) #make col names of results matrix the categories
  
  ## NOTE MATCHES AND PROBABILITY RATIOS
  unconditional_test <- t(probs)>pc # (c x n)
  identify_max <- probs==apply(probs,1,max) # (n x c)
  ratios_to_unconditional <- t(t(probs)/pc)
  max_ratios <- ratios_to_unconditional==apply(ratios_to_unconditional,1,max)
  if (all(rowSums(identify_max)==1)){ #MAXIMUM PROBABILITIES -- WITH ERROR CATCHING
    test$max_posterior <- as.vector(t(probs))[as.vector(t(identify_max))]
  } else {
    test$max_posterior <- NA
    row.picker <- rowSums(identify_max)==1
    test$max_posterior[row.picker] <- as.vector(t(probs[row.picker,]))[as.vector(t(identify_max[row.picker,]))]
    for (j in 1:length(test$max_posterior[row.picker==F])){
      test$max_posterior[row.picker==F][j] <- max(probs[row.picker==F,][j,])
    }
  }
  if (all(rowSums(max_ratios)==1)){ #fill in maximum ratios with error-catching
    test$max_ratios <- as.vector(t(ratios_to_unconditional))[as.vector(t(max_ratios))]
  } else if (sum((apply(max_ratios,1,sum)==1)==F)==1) {
    test$max_ratios[apply(max_ratios,1,sum)==1] <- as.vector(t(ratios_to_unconditional[apply(max_ratios,1,sum)==1]))[as.vector(t(max_ratios[apply(max_ratios,1,sum)==1]))]
    test$max_ratios[(apply(max_ratios,1,sum)==1)==F] <- max(ratios_to_unconditional[(apply(max_ratios,1,sum)==1)==F,])
  } else {
    test$max_ratios <- NA
    row.picker <- apply(max_ratios,1,sum)==1
    test$max_ratios[row.picker] <- as.vector(t(ratios_to_unconditional[row.picker,]))[as.vector(t(max_ratios[row.picker,]))]
    for (j in 1:length(test$max_ratios[row.picker==F])){
      test$max_ratios[row.picker==F][j] <- mean(ratios_to_unconditional[row.picker==F,][j,])
    }
  }
  name_matrix <- matrix(rep(colnames(probs),nrow(probs)),nrow=nrow(probs),ncol=ncol(probs),byrow=T)
  if (all(rowSums(identify_max)==1)){ #fill in max prob category names with error-catching
    test$max_match <- as.vector(t(name_matrix))[as.vector(t(as.matrix(identify_max)))]
  } else {
    test$max_match <- NA
    row.picker <- apply(identify_max,1,sum)==1
    test$max_match[row.picker] <- as.vector(t(name_matrix[row.picker,]))[as.vector(t(identify_max[row.picker,]))]
    for (j in 1:length(test$max_match[row.picker==F])){
      boolian.vector <- max_ratios[row.picker==F,][j,]
      # test$max_match[row.picker==F][j] <- paste(colnames(probs)[boolian.vector],collapse="; ")
      class.probs <- pc[colnames(probs)[boolian.vector]]
      test$max_match[row.picker==F][j] <- names(class.probs)[which(class.probs==min(class.probs))]
    }
  }
  if (all(rowSums(max_ratios)==1)){ #fill in max ratio category names with error-catching
    test$ratio_match <- as.vector(t(name_matrix))[as.vector(t(as.matrix(max_ratios)))]
  } else if (sum((rowSums(max_ratios)==1)==F)==1) {
    test$ratio_match <- NA
    row.picker <- apply(max_ratios,1,sum)==1
    test$ratio_match[row.picker] <- as.vector(t(name_matrix[row.picker,]))[as.vector(t(max_ratios[row.picker,]))]
    boolian.vector <- max_ratios[(row.picker)==F,]
    class.probs <- pc[colnames(probs)[boolian.vector]]
    test$ratio_match[row.picker==F] <- names(class.probs)[which(class.probs==min(class.probs))]
  } else {
  test$ratio_match <- NA
    row.picker <- rowSums(max_ratios)==1
    test$ratio_match[row.picker] <- as.vector(t(name_matrix[row.picker,]))[as.vector(t(max_ratios[row.picker,]))]
    for (j in 1:length(test$ratio_match[row.picker==F])){
      boolian.vector <- max_ratios[row.picker==F,][j,]
      class.probs <- pc[colnames(probs)[boolian.vector]]
      if(all((sum(class.probs)/length(class.probs))==class.probs)){
        test$ratio_match[row.picker==F][j] <- sample(names(class.probs), 1)
      } else {
        test$ratio_match[row.picker==F][j] <- names(class.probs)[which(class.probs==min(class.probs))]
      }
    }
  }
  return(test)
}
