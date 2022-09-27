# Support Functions
# detectRecover: function to detect time to recover from perturbation -----
detectRecover <- function(O,t) # detect time to recover mean of outcome (O) following perturbation at time (t)
{
  # Aux variables
  base<-round(mean(O[1:t-1]),2)
  O<-O[(t+1):length(O)]
  n_outcome <- length(O)
  ind_present <- O>base

  # Label panic attacks
  counter <- 1
  ind_label <- rep(NA, n_outcome)
  ind_label[1] <- 0
  for(p in 2:n_outcome) {
    if(ind_present[p]) {
      ind_label[p] <- counter
    } else {
      ind_label[p] <- 0
      if(ind_label[p-1] == counter) counter <- counter + 1
    }
  }

  ttr<-sum(ind_label==1)

  outlist <- list("ind_label" = ind_label,
                  "ttr" = ttr)
  return(outlist)

} # end of detectRecover


# detectPanic: function to detect panic attacks --------
detectPanic <- function(AF) # definition of panic attack on Arousal
{
  AF[is.na(AF)]<-mean(AF, na.rm=TRUE)
  n_panic <- length(AF)

  ind_present <- AF>=.5 # indicator: attack in progress at time step i?

  # Label panic attacks
  counter <- 1
  ind_label <- rep(NA, n_panic)
  ind_label[1] <- 0
  for(p in 2:n_panic) {
    if(!ind_present[p]) {
      ind_label[p] <- 0
      if(ind_label[p-1] == counter) counter <- counter + 1
    } else {
      ind_label[p] <- counter
    }
  }
  # Calculate Stats
  if(all(!ind_present)) counter <- 0
  panic_stats <- matrix(NA, nrow=counter, ncol=3)
  colnames(panic_stats) <- c("id", "length","severity")
  if(!all(!ind_present)) { # are there ANY panic attacks in the interval
    panic_stats[, 1] <- 1:counter
    for(p in 1:counter) {
      duration <- AF[ind_label == p]
      panic_stats[p, 2] <- length(duration) #Number of time steps above panic threshold
      panic_stats[p, 3] <- max(AF) #Amount above panic threshold
    }
  }
  # Delete final row (artifact of method of detecting panic)
  panic_stats<-panic_stats[panic_stats[,2]>0,,drop=FALSE]
  panic_count<-nrow(panic_stats)

  # Return list
  outlist <- list("ind_label" = ind_label,
                  "panic_stats" = panic_stats,
                  "n_panic" = panic_count)
  return(outlist)
}

