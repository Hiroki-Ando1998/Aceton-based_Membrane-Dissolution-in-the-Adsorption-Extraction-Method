
setwd("C:/Aceton_method")

library(ggplot2)
library(tidyr)
library(dplyr)
library(rstan)
library(bayesplot)

df <- read.csv("RealWorld_Application_SARS_PMMoV.csv")

df_ratio <- df %>% filter(!is.na(SARS_Ratio))
df_ratio_PMMoV <- df %>% filter(!is.na(PMMoV_Ratio))


y_sars <- log10(df_ratio$SARS_Ratio)
y_PMMoV <- log10(df_ratio$PMMoV_Ratio)

stan_data_sars <- list(
  N = length(y_sars),
  y = y_sars
)

stan_data_PMMoV <- list(
  N = length(y_PMMoV),
  y = y_PMMoV
)


# 3. MCMCサンプリングの実行
fit <- stan(
  file = "Bayesian_Modeling_Normal_Distribution.stan", "Bayesian_Modeling_Student-t_3,stan"
  data = stan_data_sars, #stan_data_PMMoV
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 123
)

# Estimation Results
print(fit, pars = c("mu", "sigma"), probs = c(0.025, 0.5, 0.975))
#650*350
mcmc_combo(fit, pars = c("mu", "sigma")) 
