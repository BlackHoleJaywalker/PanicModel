# September 25, 2022

# This file contains the differential equations for the formalization of the network theory of panic disorder

# -----------------------------------------
# -------- Arousal ------------------------
# -----------------------------------------

dA_dt <- function(A,
                  PT,
                  N,
                  H,
                  r_A,
                  s_PT_A) {

  r_A * ((s_PT_A * PT + N - H) - A)
}


# -----------------------------------------
# -------- Perceived Threat ---------------
# -----------------------------------------

dPT_dt <- function(PT,
                  A,
                  E,
                  r_PT,
                  k_A_PT,
                  s_E_PT,
                  h_A_PT) {

  r_PT * ((1/(1 + exp(- k_A_PT * (A - s_E_PT * E - h_A_PT)))) - PT)

}


# -----------------------------------------
# -------- Homoestatic --------------------
# -----------------------------------------

dH_dt <- function(H,
                  A,
                  r_H,
                  k_A_H,
                  h_A_H) {

  r_H * (1/(1 + exp(- k_A_H*(A - h_A_H))) - (0.5*H))

}


# -----------------------------------------
# -------- Escape -------------------------
# -----------------------------------------

dE_dt <- function(E,
                  PT,
                  X,
                  r_E,
                  k_PT_E,
                  h_PT_E,
                  TxI4) {

  r_E * ((1/(1 + exp(- k_PT_E * ((PT - X - TxI4) - h_PT_E)))) - E)

}


# -----------------------------------------
# -------- Arousal Schema -----------------
# -----------------------------------------

dS_dt <- function(S,
                  maxE,
                  maxPT,
                  cr_E_S,
                  r_S_a,
                  r_S_e) {

  # If escape has passed some threshold, change S based on levels of PT and current S
  # else, decrease (change) S in another way

  ifelse(maxE >= cr_E_S,
         r_S_a * (max(maxPT, S) - S),
         - r_S_e * S
  )

}


# -----------------------------------------
# -------- Escape Schema ------------------
# -----------------------------------------

dX_dt <- function(X,
                  maxE,
                  maxPT,
                  cr_E_X,
                  r_X_a,
                  r_X_e) {

  # If escape has passed some threshold, change escape based on levels of PT and current S
  # else, decrease (change) X in another way

  ifelse(maxE > cr_E_X,
         - r_X_e * X,
         r_X_a * (max(maxPT, X) - X)
  )

}


# -----------------------------------------
# -------- Avoidance ----------------------
# -----------------------------------------

dV_dt <- function(V,
                  S,
                  r_V,
                  k_S_V,
                  h_S_V) {

  r_V * ((1 / (1 + exp(- k_S_V * (S - h_S_V) ))) - V)

}
