\name{simPanic}
\alias{simPanic}
\title{simPanic: a computational model of panic disorder}
\description{
The simPanic function contains a computational model of panic disorder as described in "Advancing the Network Theory of Mental Disorders: A Computational Model of Panic Disorder" (<https://psyarxiv.com/km37w/>).}
\usage{
simPanic(time, stepsize=NULL, parameters = NULL,
         initial = NULL, tx = NULL, pbar = TRUE)
}
\arguments{
  \item{time}{An integer vector specifying the sequence of time steps to be simulated. For example, 0:60 will perform the simulation for 60 time steps (0 through 60). 1 time step corresponds to 1 minute of simulated time.}
  \item{stepsize}{A numeric value specifying the step size to be used in Euler's method for numerical integration. Smaller step size allows for a better approximation but is more computationally intensive. Defaults to \code{stepsize=0.01}.}
  \item{parameters}{A list specifying the parameters used for the model equations. The full list of default parameters can be found in '/R/Defaults.R'.}
  \item{initial}{A list specifying the initial values for all model components. The full list of default parameters can be found in '/R/Defaults.R'}

  \item{tx}{A list specifying the timing of 5 intervention components (I1-I5). I1 specifies the timing of psychoeducation targeting arousal schema (S). I2 specifies the timing of psychoeducation targeting escape schema (X). I3 specifies the timing of cognitive restructuring targeting arousal schema (S). I4 specifies the timing of interoceptive exposure. I5 specifies the timing of in vivo exposure. If unspecified, interventions are not delivered.}

  \item{pbar}{A logical value specifying whether to display a progress bar. Defaults to \code{pbar=TRUE}}
}




\value{
The simPanic function will produce an list with three elements:
\item{input}{A list of the initial values and parameters used in the simulation}
\item{adherence}{A vector indicating whether each assigned exposure exercise was completed (0=Not Completed; 1=Completed) when exposure therapy is administered.}
\item{outmat}{A matrix of simulation results where rows are time steps and columns are model components or parameters}
}

\author{Donald Robinaugh <d.robinaugh@northeasetern.edu>; Jonas Haslbeck <jonashaslbeck@gmail.com>; Oisin Ryan <o.ryan@uu.nl>}

\references{
Robinaugh, D., Haslbeck, J., Waldorp, L., Kossakowski, J., Fried, E.I., Millner, A., McNally, R.J., van Nes, E.H., Scheffer, M., Kendler, K.S. and Borsboom, D. 2022. Advancing the network theory of mental disorders: A computational model of panic disorder. <https://psyarxiv.com/km37w/>}


\examples{

\dontrun{

# 60 "minute" simulation with default intial values and parameters

simPanic(time = 0:60)


# 120 "minute" simulation with specified intial values and parameters

initial_specified <- list("S"= 1)
parameters_specified <- list("A" = list("r_A"= 0.55))
simPanic(time = 0:120,initial = initial_specified,parameters = parameters_specified)


# 60 "minute" simulation with specified intial values and a perturbation to arousal

perturb_time <- 10
perturb_strength <- 0.60
parameters_specified <- list("Tx" = list("minuteP" = perturb_time, "strengthP" = perturb_strength))
initial_specified <- list("S" = 0.60)
simPanic(time = 0:60, initial = initial_specified, parameters = parameters_specified)


# 5 week treatment simulation

baseline_weeks <- 0
baseline_days <- 1
tx <- list("I1" = baseline_days+baseline_weeks*7+c(1),
           "I2" = baseline_days+baseline_weeks*7+c(1),
           "I3" = (baseline_days+baseline_weeks+(1:2)*7)+c(1),
           "I4" = (baseline_days+baseline_weeks*7)+7+(1:(7*4)),
           "I5" = baseline_days+(baseline_weeks+3)*7+(1:14)[rep(c(TRUE,TRUE), 7)])
parameters_specified <- list("Tx" = list("I123_alpha" = rbeta(n = 1, 1, 9),
                                         "I4Adh" = rbeta(n = 1, 2, 2/3),
                                         "I4RdEs" = rbeta(n = 1, 2, 2/3)))
initial_specified <- list("S" = 0.80, "X" = 0.20)
time <- 0:(60*24*7*5)
simPanic(time = time, parameters = parameters_specified, initial = initial_specified, tx = tx)

}

}


