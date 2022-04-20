# jonashaslbeck@gmail.com; April 15, 2022

# --------------------------------------------------------------
# -------- What is happening here? -----------------------------
# --------------------------------------------------------------

# Some examples to showcase the package

# --------------------------------------------------------------
# -------- Simplest case ---------------------------------------
# --------------------------------------------------------------

temp <- simPanic(time = 0:100,
                 stepsize = .001)

temp$input$initial

plot(temp$outmat$A, type="l")
lines(temp$outmat$PT, col="red")

# --------------------------------------------------------------
# -------- Change parameters/initial values --------------------
# --------------------------------------------------------------


initial_spec <- list("AS"=1)
pars_spec <- list("A" = list("r_A"= 0.55))

time <- 0:(60*24*1)

temp2 <- simPanic(time = time,
                  stepsize = .001,
                  parameters = pars_spec,
                  initial = initial_spec,
                  pbar = TRUE)

plot(temp2$outmat$A, type="l", ylim=c(-.5,1))
lines(temp2$outmat$PT, col="red")

head(temp2)

out_det <- detectPanic(A = temp2$outmat$A, E = temp2$outmat$E, fear = temp2$outmat$AF, C = temp2$outmat$C)
out_det$n_panic



# --------------------------------------------------------------
# -------- Perturbation ----------------------------------------
# --------------------------------------------------------------

pars_spec <- list("Tx" = list("minuteP"= 10))
initial_spec <- list("AS" = .60)


time <- 0:60 # 1h

temp3 <- simPanic(time = time,
                  stepsize = .001,
                  parameters = pars_spec,
                  initial = initial_spec,
                  pbar = TRUE)


plot(temp3$outmat$A, type="l", ylim=c(-.5, 1))
lines(temp3$outmat$PT, col="red")
abline(v=10, col="orange", lty=2)


# --------------------------------------------------------------
# -------- Interventions ---------------------------------------
# --------------------------------------------------------------

# Code up as in supplementary materials
# 12 week baseline, then 5 week CBT
#baseline_weeks <- 12
baseline_weeks <- 2 # I just set this to zero here so I don't have to compute 12 weeks before I can test whether the intervention code works

tx <- list("I1" = baseline_weeks*7+c(1), # first day, 1st week treatment
           "I2" = baseline_weeks*7+c(1), # first day, 1st week treatment
           "I3" = (baseline_weeks+1)*7+c(1), # first day, 2nd week treatment
           "I4" = c((baseline_weeks+1)*7+1:14, (baseline_weeks+3)*7+(1:14)[rep(c(FALSE,TRUE), 7)]),
           "I5" = (baseline_weeks+3)*7+(1:14)[rep(c(TRUE,FALSE), 7)])

tx_notreat <- list("I1"=NULL,
                   "I2"=NULL,
                   "I3"=NULL,
                   "I4"=NULL,
                   "I5"=NULL)



# Check out distributions
X <- rbeta(n=1000, 1, 9) # for alpha-reduction in I 1-3; mean = 0.1
hist(X) # mean of beta = a/(a+b)
mean(X)
sd(X)
a <- 2
X <- rbeta(n=1000, a, a/3) # for adherence probability in I 4; mean = 0.6
hist(X)


#  Draw individual differences parameters
pars_spec <- list("Tx" = list("I123_alpha" = rbeta(n=1, 1, 9), # The %reduction for Interventions 1-3
                              "I4Adh" = rbeta(n=1, 2, 2/3), # Adherence probability for  Intervention 5
                              "I4RdEs" = rbeta(n=1, 2, 2/3))) # Extent to which Escape behavior is reduced

time <- 0:(60*24*7*8) # eight weeks

# Simulate
temp4 <- simPanic(time = time,
                  stepsize = .001,
                  parameters = pars_spec,
                  initial = initial_spec,
                  tx = tx,
                  pbar = TRUE)

temp4$adherence
mean(temp4$adherence)


plot(temp4$outmat$A, col="light grey", type="l", ylim=c(-.5, 1), xaxt='n')
axis(1, at = seq(1,(60*24*7*8), by = (60*24*7)), las=2,labels=0:7)
lines(temp4$outmat$AF, col="black")
lines(temp4$outmat$AS, col="red")
lines(temp4$outmat$ES, col="green")
lines(temp4$outmat$E, col="red")












max(unlist(tx))




