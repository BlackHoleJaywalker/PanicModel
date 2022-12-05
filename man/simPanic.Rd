\name{simPanic}
\alias{simPanic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to simulate data from the computational model
}
\description{
Function to simulate data from the computational model for Panic Disorder of Robinaugh and colleagues
}
\usage{
simPanic(time, stepsize, parameters, initial, tx, pbar = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{time}{
%%     ~~Describe \code{time} here~~
}
  \item{stepsize}{
%%     ~~Describe \code{stepsize} here~~
}
  \item{parameters}{
%%     ~~Describe \code{parameters} here~~
}
  \item{initial}{
%%     ~~Describe \code{initial} here~~
}
  \item{tx}{
%%     ~~Describe \code{tx} here~~
}
  \item{pbar}{
%%     ~~Describe \code{pbar} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (time, stepsize, parameters, initial, tx, pbar = TRUE)
{
    parameters <- pars_default
    if (!is.null(parameters)) {
        names_list_def <- names(pars_default)
        names_list_spec <- names(parameters)
        n_spec <- length(names_list_spec)
        for (i in 1:n_spec) {
            pars_spec_i <- parameters[[names_list_spec[i]]]
            n_pars_spec_i <- length(pars_spec_i)
            names_list_spec_j <- names(pars_spec_i)
            for (j in 1:n_pars_spec_i) {
                PS[[names_list_spec[i]]][[names_list_spec_j[j]]] <- parameters[[names_list_spec[i]]][[names_list_spec_j[j]]]
            }
        }
    }
    INI <- initial_default
    if (!is.null(initial)) {
        names_list_ini_def <- names(initial_default)
        names_list_ini_def <- names(initial)
        n_spec_ini <- length(names_list_ini_def)
        for (i in 1:n_spec_ini) {
            INI[[names_list_ini_def[i]]] <- initial[[names_list_ini_def[i]]]
        }
    }
    range_time <- range(time)
    daydef <- 60 * 24
    C_steps <- 60
    PS <- parameters
    A <- INI$A
    N <- 0
    H <- INI$H
    PT <- INI$PT
    E <- INI$E
    AF <- sqrt(A * PT)
    S <- INI$S
    X <- INI$X
    V <- 1/(1 + exp(1)^(-PS$V$k_S_V * (S - PS$V$h_S_V)))
    p_C <- 0.1/(1 + exp(1)^(PS$C$k_V_C * (V - PS$C$h_V_C)))
    C <- sample(0:1, size = 1, prob = c(1 - p_C, p_C))
    sigma <- 0.3/(1 + exp(1)^(PS$N$k_V_N * ((V) - PS$N$h_V_N))) +
        0.5
    beta <- sigma * sqrt(2/PS$N$lambda_N - 1/PS$N$lambda_N^2)
    k_A_PT <- 20 - 10 * 0.25^S + 5 * C
    h_A_PT <- 0.25^S - 0.1 * C
    outmat <- matrix(NA, nrow = length(time), ncol = 12, dimnames = list(NULL,
        c("A", "N", "H", "PT", "S", "E", "X", "AF", "V", "C",
            "p_C", "h")))
    outmat[1, ] <- c(A, N, H, PT, S, E, X, AF, V, C, p_C,
        h_A_PT)
    obs_tracker <- 2
    day_tracker <- 2
    noise_tracker <- 0
    tol <- stepsize * 0.01
    maxAF <- AF
    maxE <- E
    maxPT <- PT
    daysteps <- daydef/stepsize
    timepoints <- seq(time[1], max(time), by = stepsize)
    daypoints <- seq(time[1], max(time), by = daydef)
    if ((daydef > range_time[2]))
        day_tracker <- 1
    Nvec <- c(N, rep(NA, daysteps))
    epsilon <- rnorm(daysteps, mean = PS$N$mu_N, sd = sigma)
    for (i in 2:(daysteps + 1)) {
        Nvec[i] <- ((1 - 1/PS$N$lambda_N) * (Nvec[i - 1]) + beta *
            epsilon[i - 1])
    }
    rm(epsilon)
    if (pbar == TRUE)
        pb <- txtProgressBar(min = 1, max = length(time), initial = 1,
            char = "-", style = 3)
    for (time_tracker in 2:length(timepoints)) {
        noise_tracker <- noise_tracker + 1
        Anew <- A + dA_dt(A = A, PT = PT, N = Nvec[noise_tracker],
            H = H, r_A = PS$A$r_A, s_PT_A = PS$A$s_PT_A) * stepsize
        PTnew <- PT + dPT_dt(PT = PT, A = A, E = E, r_PT = PS$PT$r_PT,
            s_E_PT = PS$PT$s_E_PT, h_A_PT = h_A_PT) * stepsize
        Hnew <- H + dH_dt(PT = PT, A = A, E = E, r_H = PS$H$r_H,
            k_A_H = PS$H$k_A_H, h_A_H = PS$H$h_A_H) * stepsize
        Enew <- E + dE_dt(E = E, PT = PT, X = X, r_E = PS$E$r_E,
            k_PT_E = PS$E$k_PT_E, h_PT_E = PS$E$h_PT_E) * stepsize
        A <- Anew
        H <- Hnew
        PT <- PTnew
        E <- Enew
        ifelse(A > 0, AF <- sqrt(A * PT), AF <- 0)
        if (AF > maxAF)
            maxAF <- AF
        if (E > maxE)
            maxE <- E
        if (PT > maxPT)
            maxPT <- PT
        if (abs(timepoints[time_tracker]\%\%C_steps) < tol) {
            C <- sample(0:1, size = 1, prob = c(1 - p_C, p_C))
            PS$Arousal$k_A_PT <- 20 - 10 * 0.25^S + 5 * C
            PS$Arousal$h_A_PT <- 0.25^S - 0.1 * C
        }
        if (timepoints[time_tracker] == daypoints[day_tracker]) {
            if (maxAF >= PS$TS$cr_AF) {
                S_new <- S + dS_dt(S = S, maxE = maxE,
                  maxPT = maxPT, cr_E_S = PS$TS$cr_E_S, r_S_a = PS$TS$r_S_a,
                  r_S_e = PS$TS$r_S_e)
                X_new <- X + dX_dt(S = S, maxE = maxE,
                  maxPT = maxPT, cr_E_S = PS$TS$cr_E_S, r_X_a = PS$TS$r_X_a,
                  r_X_e = PS$TS$r_X_r)
                S <- S_new
                X <- X_new
                V_new <- V + dV_dt(V = V, S = S, r_V = PS$V$r_V,
                  k_S_V = PS$V$k_S_V, h_S_V = PS$V$h_S_V)
                V <- V_new
                sigma <- 0.3/(1 + exp(1)^(PS$N$k_V_N * ((V) -
                  PS$N$h_V_N))) + 0.5
                beta <- sigma * sqrt(2/PS$N$lambda_N - 1/PS$N$lambda_N^2)
                p_C <- 0.1/(1 + exp(1)^(PS$C$k_V_C * ((V) -
                  PS$C$h_V_C)))
            }
            Nvec <- c(Nvec[noise_tracker], rep(NA, daysteps))
            epsilon <- rnorm(daysteps, mean = PS$N$mu_N, sd = sigma)
            for (i in 2:(daysteps + 1)) {
                Nvec[i] <- ((1 - 1/PS$N$lambda_N) * (Nvec[i -
                  1]) + beta * epsilon[i - 1])
            }
            rm(epsilon)
            maxAF <- maxE <- maxPT <- 0
            day_tracker <- day_tracker + 1
            if (day_tracker > length(daypoints))
                day_tracker <- 1
            noise_tracker <- 0
        }
        if (time[obs_tracker] == timepoints[time_tracker]) {
            outmat[obs_tracker, ] <- c(A, N, H, PT, S, E, X,
                AF, V, C, p_C, h_A_PT)
            obs_tracker <- obs_tracker + 1
        }
        if (pbar == TRUE)
            setTxtProgressBar(pb, obs_tracker)
    }
    return(outmat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
