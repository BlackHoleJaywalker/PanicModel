# jonashaslbeck@gmail.com; Apr 4, 2018

### This function detects the onset and termination of a panic attack
### and records characteristics of the attack (e.g., attack duration)

detectPanic <- function(A,
                        E,
                        fear,
                        C) # definition of panic attack on Arousal
{
  fear[is.na(fear)]<-mean(fear, na.rm=TRUE)
  n_panic <- length(fear)

  ind_present <- fear>=.5 # indicator: attack in progress at time step i?

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
  panic_stats <- matrix(NA, nrow=counter, ncol=6)
  colnames(panic_stats) <- c("id", "length", "mean", "max","safety","C")
  if(!all(!ind_present)) { # are there ANY panic attacks in the interval
    panic_stats[, 1] <- 1:counter
    for(p in 1:counter) {
      ss_p <- A[ind_label == p] #Arousal level when there was panic
      ss_p_sb <- E[ind_label == p] #Safety behavior when there was panic
      ss_p_C <- C[ind_label == p] #Context when there was panic
      panic_stats[p, 2] <- length(ss_p) #Number of time steps above panic threshold
      panic_stats[p, 3] <- mean(ss_p) #Mean arousal while above panic threshold
      panic_stats[p, 4] <- max(ss_p) #Peak arousal while above panic threshold
      panic_stats[p, 5] <- max(ss_p_sb) #Maximum level of safety behavior while above panic threshold
      panic_stats[p, 6] <- max(ss_p_C) #Maximum level of safety behavior while above panic threshold
    }
  }

  # Delete final row (artifact of method of detecting panic)
  panic_stats<-panic_stats[-nrow(panic_stats),]

  # Return list
  outlist <- list("ind_label" = ind_label,
                  "panic_stats" = panic_stats,
                  "n_panic" = nrow(panic_stats))

  return(outlist)

} # eoF
