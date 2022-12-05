# robinaugh@gmail.com; jonashaslbeck@gmail.com; September 25, 2022

source("./R/CoreModel.R")
source("./R/Defaults.R")
source("./R/DifferentialEquations.R")
source("./R/SupportingFunctions.R")

# --------------------------------------------------------------
# -------- Illustration 1: Simulating from the model -----------
# --------------------------------------------------------------

out <- simPanic(time = 0:60,       # The simPanic function allows you to simulates the model's behavior
                stepsize = .001)   # for the specified amount of time (here, 60 "minutes" of simulated time)

                      # The output file has three components:
out$input             # (1) The initial values and parameters used in the simulation
out$adherence         # (2) Adherence to exposure exercises when a treatment is administered (See Illustration 4)
out$outmat            # (3) A matrix where rows are time steps and columns are variables
dim(out$outmat)       #     Here we have a matrix of 61 time steps (0 through 60) for 12 variables, including:
                      #     5 Fast-changing Model Components: A=Arousal; PT=Perceived Threat; E=Escape; N=Noise; H=Homeostatic Feedback
                      #     3 Slow-changing Model Components: S=Arousal Schema; X= Escape Schema; V=Avoidance
                      #     1 Emergent Component: AF=Fear
                      #     2 model parameters


# Visualize Simulation Results for Key Model Components
results <- out$outmat
plot.new()
plot.window(xlim = c(0, length(results$A)),  ylim=c(-.75, 1))
axis(1); axis(2,las=2)
mtext("Minutes", side = 1, line=3, cex=1.25)
lines(results$E, col="red",lwd=3)
lines(results$H, col="red",lty="dashed",lwd=3)
lines(results$A, col="grey",lwd=3)
lines(results$PT, col="black",lwd=3)
legend("topleft", legend=c("Arousal","Perceived Threat","Escape","Homeostatic Feedback"),
       col = c("grey","black","red","red"),
       lty = c(1,1,1,2), bty="n",cex=1.25)


# --------------------------------------------------------------
# -------- Illustration 2: Initial Values & Parameters----------
# -------------------------------------------------------------------

# The initial values of model components and parameters defining the
# relationships among model components can be changed in order to
# evaluate how the model behaves under different conditions.

# If left unspecified, the simPanic will use default initial values and parameters

# To specify initial values or parameters of interest, first create a list assigning the chosen value for the relevant component/parameter
initial_specified <- list("S"= 1)                       # Specifies an initial value of 1 for AS
parameters_specified <- list("A" = list("r_A"= .55))    # Specifies a value of .55 for the parameter r_A
                                                        # in the equation defining the rate of change for arousal A
out2 <- simPanic(time = 0:120,
                stepsize = .001,
                initial = initial_specified,            # Then use that list to define initial values (initial = )
                parameters = parameters_specified)      # and parameters (parameters = )

# Check Input Initial Values and Parameters
out2$input$initial$S # As specified, initial S = 1
out2$input$parameters$A$r_A # As specified, parameter r_A = .55

# Visualize Simulation Results for Key Model Components
results2 <- out2$outmat
plot.new()
plot.window(xlim = c(0, length(results2$A)),  ylim=c(-.75, 1))
axis(1); axis(2,las=2)
mtext("Minutes", side = 1, line = 3, cex = 1.25)
lines(results2$E, col = "red", lwd = 3)
lines(results2$H, col = "red",lty = "dashed", lwd = 3)
lines(results2$A, col = "grey",lwd = 3)
lines(results2$PT, col = "black",lwd = 3)
legend("bottomleft", legend = c("Arousal","Perceived Threat","Escape","Homeostatic Feedback"),
       col = c("grey", "black", "red", "red"),
       lty = c(1,1,1,2), bty = "n",cex = 1.25)


# --------------------------------------------------------------
# -------- Illustration 3: Building in a Perturbation ----------
# --------------------------------------------------------------

# A perturbation to Arousal can be added to the model to evaluate
# how the system responds to a perturbation of given strength at a given time

perturb_time <- 10        # Specifies that the perturbation should occur at minute 10
perturb_strength <- .60   # Specifies that the perturbation should move arousal to A=0.60

# These aspects
parameters_specified <- list("Tx" = list("minuteP" = perturb_time, "strengthP" = perturb_strength))
initial_specified <- list("S" = .60)

out3  <- simPanic(time = 0:60,
                  stepsize = .001,
                  parameters = parameters_specified,
                  initial = initial_specified,
                  pbar = TRUE)

# Visualize Simulation Results for Key Model Components
results3 <- out3$outmat
plot.new()
plot.window(xlim = c(0, length(results3$A)),  ylim = c(-.75, 1))
axis(1); axis(2, las = 2)
mtext("Minutes", side = 1, line = 3,cex = 1.25)
rect(perturb_time, -.5, (perturb_time+1), perturb_strength, col = "grey90", border = FALSE)
text(perturb_time, -0.6, "Perturbation")
lines(results3$E, col = "red", lwd = 3)
lines(results3$H, col = "red", lty = "dashed", lwd = 3)
lines(results3$A, col = "grey", lwd = 3)
lines(results3$PT, col = "black", lwd = 3)
legend("topright", legend = c("Arousal", "Perceived Threat", "Escape", "Homeostatic Feedback"),
       col = c("grey", "black", "red", "red"),
       lty = c(1,1,1,2), bty = "n")

# Note that the results of these perturbations are affected by stochastic variation in arousal.
# As a result, the response to the perturbation can be different each time the simulation is run.
# To illustrate, we can repeat the simulation 10 times and plot the trajectory of arousal for each.

N_sim <- 10
results <- list()       # Storage for the results
  for(i in 1:N_sim) {
    out <- simPanic(time = 0:60,
                    stepsize = .001,
                    parameters = parameters_specified,
                    initial = initial_specified,
                    pbar = FALSE)

    results[[i]] <- out

    if(i %in% round(seq(0, N_sim, by=5),0)){
      print(paste0("Iteration:",i,"/",N_sim," ; ",round((((i)/N_sim)*100),2),"% Complete"))
    }
  }

plot.new()
plot.window(xlim = c(0, length(results[[1]]$outmat$AF)),  ylim=c(-.75, 1))
axis(1); axis(2,las=2)
mtext("Minutes", side = 1, line = 3,cex = 1.25)
rect(perturb_time, -.5, (perturb_time+1), perturb_strength, col = "grey90", border = FALSE)
text(perturb_time,-0.6,"Perturbation")
for(i in 1:length(results)){lines(results[[i]]$outmat$AF, col = "black", lwd = .5)}
legend("topright", legend = c("Fear"),
       col = c("black"),
       lty = c(1), bty="n")

# --------------------------------------------------------------
# -------- Illustration 4: Iterating Across Parameters ---------
# --------------------------------------------------------------

# By iterating across different initial values and parameters, we can evaluate
# how the system behaves under different conditions.

# For example, we can examine how the system will respond to a range of perturbation
# strengths at different values of arousal schema (AS)

N_sim <- 100     # Specify the number of simulations you want to run per condition

perturb_strengths <- runif(N_sim,0,1)   # Generate a random set of perturbation strengths between 0 and 1
Ss <- c(.10, .50)                       # Specify the Arousal Schema (S) values you want to examine

perturb_time <- 10      # Set the time of the perturbation
time <- 0:60 # 1h       # Set the total amount of simulated time

results <- list()       # Storage for the results

for (h in 1:length(Ss)){
  initial_specified <- list("S" = Ss[h])

  for(i in 1:N_sim) {
    perturb_strength <- perturb_strengths[i]
    parameters_specified <- list("Tx" = list("minuteP"= perturb_time, "strengthP" = perturb_strength))

    out <- simPanic(time = time,
                    stepsize = .001,
                    parameters = parameters_specified,
                    initial = initial_specified,
                    pbar = FALSE)

    results[[((h-1)*N_sim)+i]] <- out

    if((((h-1)*N_sim)+i) %in% seq(0,(length(Ss)*N_sim),by=10)){
        print(paste0("Iteration:",((h-1)*N_sim)+i,"/",length(Ss)*N_sim," ; ",round(((((h-1)*N_sim)+i)/(length(Ss)*N_sim)*100),2),"% Complete"))
    }
  }
}

# Compute maximum fear (AF) in response to the perturbations in the two conditions
AFs_1 <- AFs_2 <- vector()
for (i in 1:N_sim){
  AFs_1[i] <- max(results[[i]]$outmat$AF)
  AFs_2[i] <- max(results[[((length(Ss)-1)*N_sim)+i]]$outmat$AF)
}

# Visualize Simulation Results by Perturbation Strength
plot.new()
plot.window(xlim=c(0,1),ylim=c(0,1))
axis(1); axis(2,las=2)
mtext("Perturb Strength", side = 1, line = 3, cex = 1.25)
mtext("Peak Fear", side = 2, line = 3, srt = 0, cex = 1.25)
points(perturb_strengths, AFs_1, pch = 19, col = rgb(.3, .3, .3, alpha = .3))
points(perturb_strengths, AFs_2, pch = 19, col = rgb(0, 0, 1, alpha = .3))
legend("topleft", 1, legend = c(paste("S =", Ss[1]), paste("S =", Ss[2])),
       col = c("grey", "blue"),
       pch = 19, bty = "n")

# --------------------------------------------------------------
# -------- Illustration 5: Building in Treatment ---------------
# --------------------------------------------------------------

# 5 CBT Intervention Components are built in to the model:

      # I1 = Psychoeducation targeting Arousal Schema (AS)
      # I2 = Psychoeducation targeting Escape Schema (ES)
      # I3 = Cognitive Restructuring targeting Arousal Schema (AS)
      # I4 = Interoceptive Exposure
      # I5 = In Vivo Exposure

# To incorporate the intervention components into the simulation,
# specify a baseline period (in days and in weeks) and then
# specify when the treatment components should be delivered.

baseline_weeks <- 0 # Specifies a baseline period of 0 weeks...
baseline_days <- 1  # and 1 day.

tx <- list("I1" = baseline_days+baseline_weeks*7+c(1), # Specifies that psychoeducation targeting S should occurs at Day 2
           "I2" = baseline_days+baseline_weeks*7+c(1), # Specifies that psychoeducation targeting X should occurs at Day 2
           "I3" = (baseline_days+baseline_weeks+(1:2)*7)+c(1), # Specifies that cognitive restructing targeting S should occurs at Day 9 and 16
           "I4" = (baseline_days+baseline_weeks*7)+7+(1:(7*4)),  # Specifies that interoceptive exposure should occur at Days 9-36
           "I5" = baseline_days+(baseline_weeks+3)*7+(1:14)[rep(c(TRUE,TRUE), 7)]) # Specifies that in vivi exposure should occur at Days 23-36

#  To incorporate variability in treatment efficacy and adherence, you can specify three parameters
parameters_specified <- list("Tx" = list("I123_alpha" = rbeta(n = 1, 1, 9), # Determines the efficacy (in % reduction) for I1-3
                                         "I4Adh" = rbeta(n = 1, 2, 2/3),    # Determines the adherence (in % likelihood of completing) for I4-I5
                                         "I4RdEs" = rbeta(n = 1, 2, 2/3)))  # Determines the Extent to which instruction to refrain from escape behavior
                                                                            # raises threshold at which perceived threat will lead to escape (0-1)

# Ensure there is enough time for all treatment components to be delivered
time <- 0:(60*24*7*5)

# Finally, given that this is a treatment, specify S and X values consistent with panic disorder.
initial_specified <- list("S" = .80, "X" = .20)

# Use the simPanic function to simulate the specified treatment
out4 <- simPanic(time = time,
                  stepsize = .001,
                  parameters = parameters_specified,
                  initial = initial_specified,
                  tx = tx,
                  pbar = TRUE)

out4$adherence               #  Adherence gives a vector of all assigned exposure exercises. 1=Completed. 0=Not Completed.
mean(out4$adherence)         #  The mean of this vector gives the proportion of completed exposures

# Visualize Simulation Results for Key Model Components
results4 <- out4$outmat
endbase <- (60*24*7) * baseline_weeks
endtreat <- endbase + (60*24*7)*5
exposures_x <- out4$adherence*(endbase+(7*60*24) + seq(1:length(out4$adherence))*60*24)
exposures_x[exposures_x==0] <- NA
exposures_y <- rep(0, length(exposures_x))
cog_x <- c(endbase + (7*60*24), endbase + (14*60*24))
cog_y<-rep(-.025, length(cog_x))
psyed_x <- c(endbase + (0*60*24), endbase + (0*60*24))
psyed_y <- rep(-.025, length(psyed_x))
cols <- vector()
cols[1] <- rgb(1.0, 0, 0, alpha = .05)
cols[2] <- rgb(0, 0, 1.0, alpha = .05)

plot.new()
plot.window(xlim = c(0, length(results4$A)),  ylim = c(0, 1))
# axis(1, at = seq(endbase, endtreat, by = (60*24*7)), labels = c("Session 1", "Session 2", "Session 3", "Session 4", "Session 5", "Endpoint"))
# axis(2, at = seq(0, 1, .25))
axis(1, at = seq(endbase, length(results4$A), by = (60*24*7)), labels = c("S1", "S2", "S3", "S4", "S5", "E"))
axis(2, at = seq(0, 1, .25), las = 2)
mtext("Weeks", side = 1, line = 4, cex = 1.25)
polygon(c(seq(1, length(results4$AS)), rev(seq(1,length(results4$S)))), c(results4$S, rep(0,length(results4$S))), col = cols[1], border = NA)
polygon(c(seq(1, length(results4$ES)), rev(seq(1,length(results4$X)))), c(results4$X, rep(0,length(results4$X))), col = cols[2], border = NA)
lines(results4$PT, col = "dark grey")
lines(results4$S, col = "red", lwd = 3)
lines(results4$X, col = "blue", lwd = 3)
points(exposures_x, exposures_y, pch = 19, col = "black")
points(cog_x, cog_y, pch = 15, col = "black")
points(psyed_x, psyed_y, pch = 17, col = "black")
legend("topright", 1, legend = c("Arousal Schema","Escape Schema"),
       col = c("red", "blue"),
       lty = 1, bty = "n")
legend("topleft", 1, legend = c("Psychoeducation", "Cognitive Restructuring", "Exposure Exercise"),
       pch = c(17, 15, 19), bty = "n")
