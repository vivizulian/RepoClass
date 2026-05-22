################
### Código para simular dados ###
#Definir o n amostral e simular dados para uso posterior
################


# Definir diretorio ------------------------------------------------------------

run_date <- Sys.Date()
dir <- '~/Documents/RepoClass/'


# Simular os dados -------------------------------------------------------------

#1. definir n amostral
n <- 100 # numero de sitios amostrados

#2 simular valores de variavel  preditora e erro associado
x <- rnorm(n, mean = 0, sd = 1)
beta0 <- -1.3
beta1 <- 2.5
error <- rnorm(n, mean = 0, sd = 2)

#3 criar uma variavel resposta (observada)
y <- beta0 + beta1 * x + error

#4 juntar em uma data frame
dados <- data.frame(x = x,
                    y = y)

#5 exportar os dados simulados para a pasta L0 para servir como dados coletados em campo
write.csv(dados, file = paste0(dir, 'Data/L0/dados_original-', run_date, '.csv'))


