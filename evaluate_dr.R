# jonashaslbeck@gmail.com; Feb 17, 2022

# --------------------------------------------------------------
# -------- What is happening here? -----------------------------
# --------------------------------------------------------------

# Some examples to showcase the package

# --------------------------------------------------------------
# -------- Simplest case ---------------------------------------
# --------------------------------------------------------------

temp <- simPanic(time = 0:100,
                 stepsize = .001)

plot(temp$A, type="l")
lines(temp$PT, col="red")

# --------------------------------------------------------------
# -------- Change parameters/initial values --------------------
# --------------------------------------------------------------


initial_spec <- list("AS"=.5)
pars_spec <- list("A" = list("r_A"= 0.55))

pars_spec <- list("A" = list("r_A"= 0.55, "int1"=runif(1, 0, .2)))


temp2 <- simPanic(time = 0:300,
                 stepsize = .001,
                 parameters = pars_spec,
                 initial = initial_spec,
                 pbar = TRUE)

plot(temp2$A, type="l", ylim=c(-1,1))
lines(temp2$PT, col="red")


# ---------------------------------------------------------------
# ---------------- Comparing to Prior Model ---------------------
# ---------------------------------------------------------------

initial_spec<-initial_default
pars_spec<-pars_default
initial_spec <- list("AS"=.7, "ES"=.55)

temp <- simPanic(time = 0:(60*24*7),
                  stepsize = .001,
                  parameters = pars_spec,
                  initial = initial_spec,
                  pbar = TRUE)


#Plot
par(mfrow=c(1,1))
plot.new()
plot.window(xlim=c(1,length(temp$A)), ylim=c(-1,1.5))
title(xlab = "Time", line=2)
axis(1)
axis(2, at = seq(-.5,1.5,.5),las=2)
lines(temp$A, col="grey", lwd=2) # Arousal
lines(temp$H, col="blue",lwd=1, lty="dashed") # Homoestasis
lines(temp$PT, col="black",lwd=2) # Perceived threat
lines(temp$AS, col="red", lwd=2)
lines(temp$AV, col="orange", lwd=2)
lines(temp$ES, col="green", lwd=2)

# --------------------------------------------------------------
# -------- Interventions ---------------------------------------
# --------------------------------------------------------------

# Code up as in supplementary materials
# 12 week baseline, then 5 week CBT
baseline_weeks <- 12
#baseline_weeks <- 0 # I just set this to zero here so I don't have to compute 12 weeks before I can test whether the intervention code works

tx <- list("I1" = baseline_weeks*7+c(1), # first day, 1st week treatment
           "I2" = baseline_weeks*7+c(1), # first day, 1st week treatment
           "I3" = (baseline_weeks+1)*7+c(1), # first day, 2nd week treatment
           "I4" = c((baseline_weeks+1)*7+1:14, (baseline_weeks+3)*7+(1:14)[rep(c(FALSE,TRUE), 7)]),
           "I5" = (baseline_weeks+3)*7+(1:14)[rep(c(TRUE,FALSE), 7)])


# Check out distributions
X <- rbeta(n=1000, 1, 9) # for alpha-reduction in I 1-3; mean = 0.1
hist(X) # mean of beta = a/(a+b)
a <- 2
X <- rbeta(n=1000, a, a/3) # for adherence probability in I 4; mean = 0.6
hist(X)
mean(X)

#  Draw individual differences parameters
pars_spec <- list("Tx" = list("I123_alpha" = rbeta(n=1, 1, 9), # The %reduction for Interventions 1-3
                              "I4Adh" = X <- rbeta(n=1000, 2, 2/3))) # Adherence probability for  Intervention 5

initial_spec<-initial

time <- 0:(60*24*7*18) #

# Simulate:

initial_spec<-initial_default
pars_spec<-pars_default
initial_spec <- list("AS"=.6)

temp3 <- simPanic(time = time,
                  stepsize = .001,
                  parameters = pars_spec,
                  initial = initial_spec,
                  tx = tx,
                  pbar = TRUE)



#Plot
par(mfrow=c(1,1))
plot.new()
plot.window(xlim=c(1,length(temp3$A)), ylim=c(-1,1.5))
title(xlab = "Time", line=2)
axis(1)
axis(2, at = seq(-.5,1.5,.5),las=2)
lines(temp3$A, col="grey", lwd=2) # Arousal
lines(temp3$H, col="blue",lwd=1, lty="dashed") # Homoestasis
lines(temp3$PT, col="black",lwd=2) # Perceived threat
lines(temp3$AS, col="red", lwd=2)
lines(temp3$AV, col="orange", lwd=2)
lines(temp3$ES, col="green", lwd=2)


# Simulate:

initial_spec<-initial_default
pars_spec<-pars_default
initial_spec <- list("AS"=.5)

temp4 <- simPanic(time = time,
                  stepsize = .001,
                  parameters = pars_spec,
                  # initial = initial_spec,
                  tx = tx,
                  pbar = TRUE)



par(mfrow=c(1,1))
plot.new()
plot.window(xlim=c(1,length(temp3$A)), ylim=c(-1,1.5))
title(xlab = "Time", line=2)
axis(1)
axis(2, at = seq(-.5,1.5,.5),las=2)
lines(temp3$A, col="grey", lwd=2) # Arousal
lines(temp3$H, col="blue",lwd=1, lty="dashed") # Homoestasis
lines(temp3$PT, col="black",lwd=2) # Perceived threat
lines(temp3$AS, col="red",lwd=3) # Perceived threat
lines(temp3$AV, col="orange", lty="dashed",lwd=2) # Perceived threat
lines(temp3$ES, col="green",lwd=3) # Perceived threat
lines(temp3$E, col="blue",lwd=3) # Perceived threat


par(mfrow=c(1,1))
plot.new()
plot.window(xlim=c(1,length(temp3$A)), ylim=c(-1,1.5))
title(xlab = "Time", line=2)
axis(1)
axis(2, at = seq(-.5,1.5,.5),las=2)
lines(temp3$A, col="grey", lwd=2) # Arousal
lines(temp4$H, col="blue",lwd=1, lty="dashed") # Homoestasis
lines(temp4$PT, col="black",lwd=2) # Perceived threat
lines(temp4$AS, col="red",lwd=3) # Perceived threat
lines(temp4$AV, col="orange", lty="dashed",lwd=3) # Perceived threat
lines(temp4$ES, col="green",lwd=3) # Perceived threat
lines(temp4$E, col="purple",lwd=3) # Perceived threat




