################
### Código modelo de organizacao ###
#1. Adiciono todos os pacotes necessarios.
#2. Escolho o diretorio (pasta) que vou trabalhar.
#3. Leio os dados que vou usar no codigo.
#4. Processo, limpo ou analiso os dados
#5. Exporto o que necessito (dados processados ou resultados)
#6. Traducao para analise em abordagem bayesiana
################


# 1.load packages ----------------------------------------------------------

library(rjags)
library(MCMCvis)


# 2.set directory ------------------------------------------------------------

run_date <- Sys.Date()
dir <- '~/Documents/RepoClass/'


# 3.read in data -------------------------------------------------------------

dados <- read.csv(paste0(dir, 'Data/L0/dados_original.csv'))


# 4.process or analyze the data

# ajustar um modelo linear
model <- lm(y ~ x, data = dados)

# ver resultados
summary(model)

# plot com resultados
plot(dados$x, dados$y,
     pch = 16,
     xlab = "x",
     ylab = "y")

abline(model, col = "red", lwd = 2)


# 5.save out ----------------------------------------------------------------

resultados <- list(data = dados,  #dados usados
                   model = model, #modelo usado
                   summary = summary(model), #resumo dos resultados
                   estimativas = broom::tidy(model),
                   residuos = broom::glance(model))

saveRDS(resultados, file = paste0(dir, 'Results/ML1_', run_date, '.rds'))





# 6.traduzir o mesmo processo para abordagem bayesiana ----------------------------------------------------------------

#Referência:
#https://cran.r-project.org/web/packages/MCMCvis/vignettes/MCMCvis.html

#construir o modelo em jags

modelo_linear <- "
model {

  # Likelihood
  for(i in 1:N){
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta0 + beta1 * x[i]
  }

  # Priors
  beta0 ~ dnorm(0, 0.001)
  beta1 ~ dnorm(0, 0.001)
  sigma ~ dunif(0, 100)
  tau <- pow(sigma, -2)
}
"


jags_data <- list(y = dados$y,
                  x = dados$x,
                  N = nrow(dados))

inits <- function() {list(beta0 = 0, beta1 = 0, sigma = 1)}

params <- c("beta0", "beta1")

resultados <- jags(data = jags_data,
                   inits = inits,
                   parameters.to.save = params,
                   model.file = textConnection(modelo_linear),
                   n.chains = 3,
                   n.iter = 500,
                   n.burnin = 100,
                   n.thin = 2)



# visualizar resultados 

MCMCvis::MCMCsummary(resultados, round = 2)

MCMCvis::MCMCsummary(resultados, 
                     params = 'beta1', 
                     Rhat = TRUE, 
                     n.eff = TRUE, 
                     probs = c(0.1, 0.5, 0.9), 
                     round = 2)

MCMCvis::MCMCtrace(resultados, 
                   params = c('beta1'), 
                   ISB = FALSE, 
                   exact = TRUE,
                   pdf = FALSE)

MCMCvis::MCMCplot(resultados, 
                  params = c('beta0', 'beta1'), 
                  ci = c(50, 90))

MCMCvis::MCMCplot(resultados, 
                  params = c('beta0', 'beta1'), 
                  xlab = 'My x-axis label',
                  main = 'MCMCvis plot',
                  labels = c('Intercept', 'Preditora'),
                  col = c('purple', 'orange'),
                  sz_labels = 1.5,
                  sz_med = 2,
                  sz_thick = 7,
                  sz_thin = 3,
                  sz_ax = 4,
                  sz_main_txt = 2)


# exportar os resultados para a pasta

MCMCvis::MCMCdiag(resultados,
                  round = 3,
                  file_name = paste0('ML_jags-', run_date),
                  dir = paste0(dir),
                  mkdir = paste0('Results/ML_jags-', run_date),
                  pg0 = TRUE,
                  save_obj = TRUE,
                  obj_name = paste0('model_fit-', run_date),
                  add_obj = list(dados),
                  add_obj_names = list(paste0('data-', run_date)),
                  cp_file = paste0(dir, 'Scripts/1a_modelo_resultados.R'),
                  cp_file_names = paste0('1a_modelo_resultados_', run_date, '.R'))



