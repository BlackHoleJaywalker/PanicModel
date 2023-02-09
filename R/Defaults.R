# December, 2022

# -----------------------------------------
# -------- Default: Parameters ------------
# -----------------------------------------

pars_default <- list("A" = list("r_A"= 0.5,
                                "s_PT_A" = 1),

                     "H" = list("r_H" = 0.05,
                                "k_A_H" = 20,
                                "h_A_H" = 0.4),

                     "PT" = list("r_PT" = 1,
                                 "s_E_PT" = 0.25),

                     "E" = list("r_E" = 0.25,
                                "k_PT_E" = 10,
                                "h_PT_E" = 0.5,
                                "TxI4"=0),

                     "V" = list("r_V" = 1,
                                 "k_S_V" = 15,
                                 "h_S_V" = 0.75),

                     "N" = list("mu_N" = 0,
                                "lambda_N" = 60,
                                "k_V_N" = 5,
                                "h_V_N" = 0.5),

                     "C" = list("k_V_C" = 5,
                                "h_V_C" = 0.5,
                                "cr_E_C" = 0.5,
                                "C_steps" = 60),

                     "TS" = list("cr_AF" = 0.50,
                                 "cr_E_S" = 0.25,
                                 "cr_E_X" = 0.25,
                                 "r_S_a" = 0.25,
                                 "r_S_e" = 0.10,
                                 "r_X_a" = 0.10,
                                 "r_X_e" = 0.25),

                     "Tx" = list("I123_alpha" = 0.1,
                                 "I4Adh" = 0.6,
                                 "I4RdEs" = 0.6,
                                 "strengthP"=0.5))


# -----------------------------------------
# -------- Default: Initial Values --------
# -----------------------------------------

initial_default <- list("A" = 0,
                        "S" = 0.5,
                        "H" = 0,
                        "PT" = 0,
                        "E" = 0,
                        "X" = 0.5)

# -----------------------------------------
# -------- Default: Simulation Values --------
# -----------------------------------------

sim_default <- list("stepsize" = .001)
