### d.robinaugh@northeasetern.edu; jonashaslbeck@gmail.com; o.ryan@uu.nl; December 2022

simPanic <- function(time, # integer vector 1:n, indicating the time interval, where 1 is one "minute"
                     stepsize = NULL, # stepsize; <= 1
                     parameters = NULL, # specify parameter values for the model
                     initial = NULL, # specify initial values for state variables
                     tx = NULL, # specify any interventions to occur during the simulation
                     pbar = TRUE) # progress bar

{

  # Setup Step 0: Overwrite default with specified parameters/initial values

  ## Overwrite default parameters, if specified ------
  PS <- pars_default

  # Parameters specified?
  if (!is.null(parameters)) {

    names_list_def <- names(pars_default)
    names_list_spec <- names(parameters)
    n_spec <- length(names_list_spec)

    # Loop over specified upper level list entries
    for (i in 1:n_spec) {
      pars_spec_i <- parameters[[names_list_spec[i]]]
      n_pars_spec_i <- length(pars_spec_i)
      names_list_spec_j <- names(pars_spec_i)
      for(j in 1:n_pars_spec_i) {
        PS[[names_list_spec[i]]][[names_list_spec_j[j]]] <- parameters[[names_list_spec[i]]][[names_list_spec_j[j]]]
      }
    }
  } # end: if

  ## Overwrite default starting values, if specified ------
  INI <- initial_default

  # Initial values specified?
  if (!is.null(initial)) {

    names_list_ini_def <- names(initial_default)
    names_list_ini_def <- names(initial)
    n_spec_ini <- length(names_list_ini_def)

    for (i in 1:n_spec_ini) {
      INI[[names_list_ini_def[i]]] <- initial[[names_list_ini_def[i]]]
    }

  } # end: if

  ## Specify simulation parameters, if not previously specified
  if (is.null(stepsize)) {
    stepsize<-sim_default$stepsize
  } # end: if

  # Setup Step 1: Import Time Scale and Default Parameters --------

  range_time <- range(time) # Range of time steps, 1:number of specified iterations
  daydef <- 60*24 # How many units of time do we update the "slow" variables? 1 Day (60 minutes * 24 hours)
  # PS$C$C_steps <- 60 # update the PT-related parameters (context/ situation variables) every hour


  # Setup Step 2: Specify Components ---------

  # Set initial values
  A  <- INI$A # arousal
  N  <- 0 # noise initial draw is zero
  H  <- INI$H # homeostatic feedback
  PT <- INI$PT # perceived threat
  E  <- INI$E # escape

  # fear
  if (A > 0) {
    AF <- sqrt(A * PT)
  } else {
    AF <- 0
  }

  S <- INI$S # arousal schema
  X <- INI$X # escape schema
  V <- 1 / (1 + exp(1)^(-PS$V$k_S_V * (S - PS$V$h_S_V))) # avoidance

  # context
  p_C <- 0.1 / (1 + exp(1)^(PS$C$k_V_C * (V - PS$C$h_V_C)))
  C <- sample(0:1, size = 1, prob = c(1-p_C, p_C))

  # Track Adherence
  v_Adh <- c()

  # Setup Step 3: Specify Additional Parameters ---------

  # Additional Parameters

  # Arousal
  sigma <- 0.30 / (1 + exp(1)^(PS$N$k_V_N * ((V) - PS$N$h_V_N))) + 0.50
  beta <-  sigma * sqrt(2/PS$N$lambda_N - 1/PS$N$lambda_N^2)
  sigma <- sigma * sqrt(0.001)

  # Initial Perceived Threat parameters - updated every hour
  PS$PT$k_A_PT <- 20 - 10 * 0.10^S + 5 * C
  PS$PT$h_A_PT <- 0.25^S - 0.1 * C # Update parameters determined by arousal schema

  # Create storage
  outmat <- matrix(NA, nrow = length(time), ncol = 12,
                   dimnames = list(NULL,c("A","N","H","PT","S","E","X","AF","V","C","p_C","h") ))

  # Save initial values
  outmat[1, ] <- c(A, N, H, PT, S, E, X, AF, V, C, p_C, PS$PT$h_A_PT)

  # Track Time with a time-tracker
  obs_tracker <- 2 # the first observation has already been taken, so next save point is the 2nd

  # Introduce a "day_tracker" and a "noise_tracker"
  day_tracker <- 2 # This starts at 2 because we have already draw the noise for day 1 below
  noise_tracker <- 0
  tol <- stepsize *.01 # sets a tolerance for evaluating time equalities

  # Track "maximum" values of fear (AF), escape (E) and perceived theat (PT) WITHIN A DAY for end-of-day updating
  maxAF <- AF
  maxE  <- E
  maxPT <- PT

  # Draw Noise for a day at a time
  daysteps <- daydef / stepsize # how many euler steps in a day
  timepoints <- seq(time[1], max(time), by = stepsize) # what time points do all euler steps occur at?
  daypoints <- seq(time[1], max(time), by = daydef) # what time points do the day-changes occur at?

  # If the time range doesn't encapsulate a day, re-set day_tracker
  if ((daydef > range_time[2])) day_tracker <- 1

  # Draw noise for a day at a time
  Nvec <- c(N, rep(NA, daysteps))
  sigma_scaled <- sigma / sqrt(stepsize)
  epsilon <- rnorm(daysteps, mean = PS$N$mu_N, sd = sigma_scaled)

  for (i in 2:(daysteps+1)){
    Nvec[i] <- (1 - 1/PS$N$lambda_N) * (Nvec[i-1]) + beta*epsilon[i-1]
  }
  # Nvec <- Nvec * sqrt(stepsize)
  rm(epsilon)


  # Simulation Step 1: 'For Loop' that carries out simulation---------

  # Setup progress bar
  if (pbar==TRUE) pb <- txtProgressBar(min = 1, max = length(time), initial = 1, char = "-", style = 3)

  # Changed to a for loop
  for (time_tracker in 2:length(timepoints)) {

    noise_tracker <- noise_tracker + 1

    # SimStep 1A: Fast Model Situations ----------

    # Update other variables based on past
    # Could be made more efficient with ordering updates but this is the safest approach
    # due to presence of cyclic relationships (if acyclic just start at the exogenous and work down)

    ## Create new values

    # Arousal
    Anew <- A + dA_dt(A = A,
                      PT = PT,
                      N = Nvec[noise_tracker],
                      H = H,
                      r_A = PS$A$r_A,
                      s_PT_A = PS$A$s_PT_A) * stepsize

    #if (time_tracker == 9998) {
   #   browser()
  #  }


    # Minute-intervention on Arousal
    if (!is.null(PS$Tx$minuteP)) {
      if (time_tracker == (PS$Tx$minuteP / stepsize)) Anew <- PS$Tx$strengthP
    }

    # Perceived Threat
    PTnew <- PT + dPT_dt(PT = PT,
                         A = A,
                         E = E,
                         r_PT = PS$PT$r_PT,
                         k_A_PT = PS$PT$k_A_PT,
                         s_E_PT = PS$PT$s_E_PT,
                         h_A_PT = PS$PT$h_A_PT) * stepsize


    # Homoestatic
    Hnew <- H + dH_dt(H = H,
                      A = A,
                      r_H = PS$H$r_H,
                      k_A_H = PS$H$k_A_H,
                      h_A_H = PS$H$h_A_H) * stepsize

    # Escape
    Enew <- E + dE_dt(E = E,
                      PT = PT,
                      X = X,
                      r_E = PS$E$r_E,
                      k_PT_E = PS$E$k_PT_E,
                      h_PT_E = PS$E$h_PT_E,
                      TxI4 = PS$E$TxI4) * stepsize

    # If Escape larger than threshold, P(Context=1)=0
    if(E > PS$C$cr_E_C) C <- 0

    # Overwrite current values
    A  <- Anew
    H  <- Hnew
    PT <- PTnew
    E  <- Enew

    # AF updated based on current
    if (A > 0) {
      AF <- sqrt(A * PT)
    } else {
      AF <- 0
    }

    # update maxAF, maxE and maxPT so far today
    if (AF > maxAF) maxAF <- AF
    if (E > maxE) maxE <- E
    if (PT > maxPT) maxPT <- PT

    # SimStep 1B: Slow Model Interactions -----------

    # ------- Hour-level: Update --------

    if (abs(timepoints[time_tracker] %% PS$C$C_steps) < tol) {

      # Update C, the situation
      C <- sample(0:1, size = 1, prob = c(1 - p_C, p_C))

      PS$PT$k_A_PT <- 20 - 10 * 0.1^S + 5 * C # update parameters determined by arousal schema
      PS$PT$h_A_PT <- 0.25^S - 0.1 * C # update parameters determined by arousal schema

    } # end if: situation update


    # ------- Day-level: Update --------

    if (timepoints[time_tracker] == daypoints[day_tracker]) {  # Is it the end of a Day?

      # S, X, and V can only change if maxAF is above a critical threshold
      if (maxAF >= PS$TS$cr_AF) { #

        # Update Arousal Schema
        S_new <- S + dS_dt(S = S,
                           maxE = maxE,
                           maxPT = maxPT,
                           cr_E_S = PS$TS$cr_E_S,
                           r_S_a = PS$TS$r_S_a,
                           r_S_e = PS$TS$r_S_e)

        # Update Escape Schema
        X_new <- X + dX_dt(X = X,
                           maxE = maxE,
                           maxPT = maxPT,
                           cr_E_X = PS$TS$cr_E_X,
                           r_X_a = PS$TS$r_X_a,
                           r_X_e = PS$TS$r_X_e)

        # Overwrite current values
        S <- S_new
        X <- X_new

      } # end of if AF statement

      # Update Avoidance
      V_new <- V + dV_dt(V = V,
                         S = S,
                         r_V = PS$V$r_V,
                         k_S_V = PS$V$k_S_V,
                         h_S_V = PS$V$h_S_V)

      # Overwrite current values
      V <- V_new

      # Update Noise draw parameters
      sigma <- 0.30 / (1 + exp(PS$N$k_V_N * ((V) - PS$N$h_V_N))) + 0.50
      beta <-  sigma * sqrt(2 / PS$N$lambda_N - 1 / PS$N$lambda_N ^ 2)
      sigma <- sigma * sqrt(0.001) # May 2nd, 22: This is done so that the model is still calibrated after adding the proper wiener scaling sqrt(timestep)

      # Update Situation
      p_C <- 0.1 / (1 + exp(PS$C$k_V_C * ((V) - PS$C$h_V_C)))

      # ----- Apply Intervention ------

      if(!is.null(tx)) { # if there are interventions

        # Save old value on E-parameter
        if(day_tracker %in% 1:2) TxI4_old <- PS$E$TxI4

        # Intervention 1: Psychoeducation on S
        if(day_tracker %in% tx$I1) S <- S*(1-PS$Tx$I123_alpha) # decrease, since lower S values are good

        # Intervention 2: Psychoeducation on X
        if(day_tracker %in% tx$I2) X <- X + PS$Tx$I123_alpha*(1-X) # increase, since higher X values are good

        # Intervention 3: Cognitive Restructuring on S
        if(day_tracker %in% tx$I3) S <- S*(1-PS$Tx$I123_alpha)

        # Intervention 4: Interoceptive Exposure
        if(day_tracker %in% tx$I4) {

          # 4.1 Pulse intervention on Arousal, with increasing intensity
          # Hardcoded: different intensity of pulses in the four weeks
          if(day_tracker %in% tx$I4[1:7]) A_pulse <- 0.25
          if(day_tracker %in% tx$I4[8:14]) A_pulse <- 0.50
          if(day_tracker %in% tx$I4[15:18]) A_pulse <- 0.75
          if(day_tracker %in% tx$I4[19:21]) A_pulse <- 1

          ind_expo <- sample(0:1, size=1, prob = c(1-PS$Tx$I4Adh, PS$Tx$I4Adh))
          if(ind_expo==1) A <- A_pulse

          v_Adh <- c(v_Adh, ind_expo)

          # Reduce Escape behavior
          PS$E$TxI4 <- PS$Tx$I4RdEs
        }

        # Intervention 5:
        if(day_tracker %in% tx$I5) {
          # Switch Context on for 1 h (note: holds only for 1h, because after it will be overwritten by the 1h loop above)
          if(ind_expo==1){
            C <- 1
            PS$PT$k_A_PT <- 20 - 10 * 0.1^S + 5 * C
            PS$PT$h_A_PT <- 0.25^S - 0.1 * C}
        }

        # One week after last intervention: set E-parameter back
        if(!is.null(tx$I5)) if(day_tracker == max(tx$I5 + 7)) PS$E$TxI4 <- TxI4_old

      } # end if: interventions


      # Draw noise for the next week again, remove epsilon because it's not needed
      Nvec <- c(Nvec[noise_tracker], rep(NA,daysteps))

      sigma_scaled <- sigma / sqrt(stepsize)
      epsilon <- rnorm(daysteps, mean = PS$N$mu_N, sd = sigma_scaled)

      for(i in 2:(daysteps+1)){
        Nvec[i] <- ((1 - 1/PS$N$lambda_N) * (Nvec[i-1]) + beta*epsilon[i-1])
      }
      # Nvec <- Nvec * sqrt(stepsize)
      rm(epsilon) # Note: Remove Epsilon

      # Re-set maximum day-trackers (set to "floor" of these variables)
      maxAF <- maxE <- maxPT <- 0

      # Re-set the noise_tracker and change day tracker
      day_tracker <- day_tracker + 1
      if(day_tracker > length(daypoints)) day_tracker <- 1
      noise_tracker <- 0

    } # end if: day update

    # Should we save the current values of all variables to the output?
    if(time[obs_tracker] == timepoints[time_tracker]){ # is current time equal to a "save-point" time?
      outmat[obs_tracker,] <- c(A, N, H, PT, S, E, X, AF, V, C, p_C, PS$PT$h_A_PT)
      # update observation tracker
      obs_tracker <- obs_tracker + 1
    }

    if(pbar==TRUE) setTxtProgressBar(pb, obs_tracker)

  } # End: for loop over time steps


  # Simulation Step 2: Specify Function's Output ---------

  outmat <- as.data.frame(outmat)

  outlist <- list("outmat" = outmat,
                  "adherence" = v_Adh,
                  "input" = list("parameters" = PS,
                                 "initial" = INI))
  return(outlist)

} # End of Function


