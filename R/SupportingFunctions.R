# October,2023

# detectRecover: function to detect time to recover from perturbation -----
detectRecover <- function(O,t) # detect time to recover mean of outcome (O) following perturbation at time (t)
{
  # Aux variables
  base <- round(mean(O[1:t]),2)
  base_sd<-round(sd(O[1:t]),2)
  n_outcome <- length(O)
  ind_present <- O>(base+base_sd*2)

  # Label response
  counter <- 1
  ind_label <- rep(NA, n_outcome)
  ind_label[1:t] <- 0
  for(p in (t+1):n_outcome) {
    if(ind_present[p]) {
      ind_label[p] <- counter
    } else {
      ind_label[p] <- 0
      if(ind_label[p-1] == counter) counter <- counter + 1
    }
  }

  ttr <- sum(ind_label == 1)

  outlist <- list("ind_label" = ind_label,
                  "ttr" = ttr)
  return(outlist)

} # end of detectRecover


# detectPanic: function to detect panic attacks --------
detectPanic <- function(out,t=NA) # definition of panic attack on Arousal
{
  A<-out$outmat$A
  AF<-out$outmat$AF
  strengthP<-out$input$parameters$Tx$strengthP

  if(is.null(out$input$parameters$Tx$minuteP)==TRUE){minuteP<-NA
  strengthP<-NA
  }else{minuteP<-out$input$parameters$Tx$minuteP
  }


  AF[is.na(AF)] <- mean(AF, na.rm=TRUE)
  A[is.na(A)] <- mean(A, na.rm=TRUE)

  n <- length(AF)

  ind_present <- AF >= 0.5 # indicator: attack in progress at time step i?

  # Label episodes of elevated fear
  counter <- 1
  ind_label <- rep(NA, n)
  ind_label[1:10] <- 0
  for (p in 11:n) {
    if (!ind_present[p]) {
      ind_label[p] <- 0
      if (ind_label[p-1] == counter) counter <- counter + 1
    } else {
      ind_label[p] <- counter
    }
  }

  # Calculate Stats
  episodes <- max(ind_label)
  panic_stats <- matrix(NA, nrow = episodes, ncol = 10)
  colnames(panic_stats) <- c("id", "duration","fear_base","perturb_strength","perturb_time","fear_peak","fear_peak_time","fear_surge","arousal_peak","panic")

  if (!all(!ind_present)) { # are there ANY panic attacks in the interval
    for (p in 1:episodes) {
      duration <- length(AF[ind_label == p])
      peak<-max(AF[ind_label==p])
      peaki<-which.max(AF[ind_label==p])-1+which(ind_label==p)[1]
      base<-AF[(peaki-10)]
      surge<-peak/base
      peak_a<-max(A[ind_label==p])
      panic_stats[p, 1] <- p
      panic_stats[p, 2] <- duration #Number of time steps above fear threshold
      panic_stats[p, 3] <- base #fear 10 minutes prior to peak
      panic_stats[p, 4] <- strengthP
      panic_stats[p, 5] <- minuteP
      panic_stats[p, 6] <- peak #max fear
      panic_stats[p, 7] <- peaki #max fear time
      panic_stats[p, 8] <- surge #peak/base
      panic_stats[p, 9] <- peak_a #max arousal
      panic_stats[p, 10] <- as.numeric(surge>=2 & peak_a>.25) #Elevated fear and arousal, surging (doubling) to peak within 10 minutes
    }
  }

  panic_stats <- panic_stats[panic_stats[,2] > 0, ,drop=FALSE]
  panic_count <- sum(panic_stats[,10])

  # Return list
  outlist <- list("ind_label" = ind_label,
                  "panic_stats" = panic_stats,
                  "n_panic" = panic_count)
  return(outlist)
}
