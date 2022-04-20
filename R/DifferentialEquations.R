# October 28, 2021

# This file contains the differential equations for all

# -----------------------------------------
# -------- Arousal ------------------------
# -----------------------------------------

d_A_dt <- function(A,
                   PT,
                   N,
                   H,
                   r_A,
                   s_PT_A) {

  s_dAdt <- r_A * ((s_PT_A*PT + N - H) - A)

  return(s_dAdt)

}


# -----------------------------------------
# -------- Perceived Threat ---------------
# -----------------------------------------

d_PT_dt <- function(PT,
                    A,
                    E,
                    r_PT,
                    k_A_PT,
                    s_E_PT,
                    h_A_PT) {

  s_dPTdt <- r_PT * ((1/(1 + exp(- k_A_PT*(A - (s_E_PT*E) - h_A_PT)))) - PT)

  return(s_dPTdt)

}


# -----------------------------------------
# -------- Homoestatic --------------------
# -----------------------------------------

d_H_dt <- function(H,
                   A,
                   r_H,
                   k_A_H,
                   h_A_H) {

  s_dHdt <- r_H * (1/(1 + exp(- k_A_H*(A - h_A_H))) - (0.5*H))

  return(s_dHdt)

}


# -----------------------------------------
# -------- Escape -------------------------
# -----------------------------------------

d_E_dt <- function(E,
                   PT,
                   ES,
                   r_E,
                   k_PT_E,
                   h_PT_E,
                   TxI4) {

  s_dEdt <- r_E * ((1/(1 + exp(- k_PT_E * ((PT - ES - TxI4) - h_PT_E)))) - E)

  return(s_dEdt)

}


# -----------------------------------------
# -------- Arousal Schema -----------------
# -----------------------------------------

d_AS_dt <- function(AS,
                    maxE,
                    maxPT,
                    cr_E_AS,
                    r_AS_a,
                    r_AS_e) {

  # If escape has passed some threshold, change AS based on levels of PT and current AS
  # else, decrease (change) AS in another way

  ifelse(maxE > cr_E_AS,
         s_dASdt <- + r_AS_a * (max(maxPT, AS) - AS),
         s_dASdt <- - r_AS_e * AS
  )

  return(s_dASdt)

}


# -----------------------------------------
# -------- Escape Schema ------------------
# -----------------------------------------

d_ES_dt <- function(ES,
                    maxE,
                    maxPT,
                    cr_E_ES,
                    r_ES_a,
                    r_ES_e) {

  # If escape has passed some threshold, change escape based on levels of PT and current AS
  # else, decrease (change) ES in another way

  ifelse(maxE > cr_E_ES,
         s_dESdt <- - r_ES_e * ES,
         s_dESdt <- + r_ES_a * (max(maxPT, ES) - ES)
  )

  return(s_dESdt)

}


# -----------------------------------------
# -------- Avoidance ----------------------
# -----------------------------------------

d_AV_dt <- function(AV,
                    AS,
                    r_AV,
                    k_AS_AV,
                    h_AS_AV) {

  s_dAVdt <- r_AV * ((1 / (1 + exp(- k_AS_AV * ( (AS) - h_AS_AV) ))) - AV)

  return(s_dAVdt)

}










