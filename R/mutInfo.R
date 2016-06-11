mutInfo <- function(coding, train_matrix){
  ## Modified mutual information feature selection algorithm
  ## from McCallum and Nigam (1998)
  nc <- as.vector(table(coding)) #number of training obs per category (c x 1)
  names(nc) <- names(table(coding)) #naming nc vector with category names
  theta_c <- nc/nrow(train_matrix) #simple prior probs of categories (c x 1)
  theta_j <- colSums(train_matrix)/nrow(train_matrix) #word probs
  coding <- coding
  
  ##Reordering these vectors to deal with the reference category problem
  ##If the reference category is the least common category, predictive accuracy is better
  nc <- nc[order(theta_c, decreasing=T)]
  theta_c <- theta_c[order(theta_c, decreasing=T)] #sorting nc and theta_c by theta_c
  
  ##Calculating priors over terms/categories
  njc <- matrix(NA, nrow=length(unique(coding)), ncol=ncol(train_matrix)) #word frequencies by category (c x j)
  rownames(njc) <- names(nc); colnames(njc) <- colnames(train_matrix) #apply category names and term names to dimensions
  for(cat in 1:length(unique(coding))){ #loop over categories to count this
    if(length(coding[coding==rownames(njc)[cat]])>1) {
      njc[cat,] <- colSums(train_matrix[coding==rownames(njc)[cat],])
    } else {
      njc[cat,] <- as.vector(train_matrix[coding==rownames(njc)[cat],])
    }
  }
  pjc <- njc/nrow(train_matrix)
  all_j <- pjc * log(pjc/ (theta_c %*% t(theta_j)))
  avg.mut.info <- apply(all_j, 2, function(x) sum(x, na.rm=T))
  return(avg.mut.info)
}
