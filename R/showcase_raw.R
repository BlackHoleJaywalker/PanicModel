source("R/simPanic.R")
source("R/Defaults.R")
source("R/SupportingFunctions.R")
source("R/DifferentialEquations.R")

perturb_time <- 10
perturb_strength <- 0.60

parameters_specified <- list("Tx" = list("minuteP" = perturb_time, "strengthP" = perturb_strength))
initial_specified <- list("S" = 0.60)

browser()

out3  <- simPanic(time = 0:60,
                  initial = initial_specified,
                  parameters = parameters_specified,
                  pbar = TRUE)

results3 <- out3$outmat


plot.new()
plot.window(xlim = c(0, length(results3$A)),  ylim = c(-.75, 1))
axis(1); axis(2, las = 2)
mtext("Minutes", side = 1, line = 3,cex = 1.25)
rect(perturb_time, -0.5, (perturb_time+1), perturb_strength, col = "grey90", border = FALSE)
text(perturb_time, -0.6, "Perturbation")
lines(results3$E, col = "red", lwd = 3)
lines(results3$H, col = "red", lty = "dashed", lwd = 3)
lines(results3$A, col = "grey", lwd = 3)
lines(results3$PT, col = "black", lwd = 3)
legend("topright", legend = c("Arousal", "Perceived Threat", "Escape", "Homeostatic Feedback"),
       col = c("grey", "black", "red", "red"),
       lty = c(1,1,1,2), bty = "n")


