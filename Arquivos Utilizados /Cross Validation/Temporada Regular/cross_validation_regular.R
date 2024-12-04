source("dados_regular.R")

library(caret)
set.seed(236292)

#Vamos mudar o nome porque estava dando erro na hora do gamlss
dados_cross <- rename(dados_regressao, TresPA = `3PA`)

#### Cross Validation K Fold #########
# O processo envolve dividir o conjunto de dados em 5 partes (ou folds) e,
# em seguida, iterar sobre esses folds. Em cada iteração, um dos folds é usado 
# como conjunto de teste e os outros folds são usados como conjunto de 
# treinamento. Essa é a essência da validação cruzada.

#No entanto, ao invés de fazer isso apenas uma vez, o código original executa
# a validação cruzada 5 vezes (ou seja, 5 iterações externas), o que é 
# conhecido como "validação cruzada repetida". Cada repetição é essencialmente 
# uma validação cruzada separada. Isso ajuda a reduzir a variabilidade dos 
# resultados, fornecendo uma estimativa mais robusta do desempenho do modelo.


# Definindo os modelos
# Primeiro, você define os modelos que deseja avaliar e as métricas de 
# desempenho que deseja calcular. Os modelos são definidos como uma lista de 
# objetos de modelo, onde a chave é o nome do modelo e o valor é o modelo 
# em si. As métricas são definidas como um vetor de strings contendo os nomes 
# das métricas que você deseja calcular.

modelos <- list(
  "regressao_linear" = lm(formula = WINP ~ TEAM + PF + FGP + FGM + PlusMinus, data = dados_cross),
  "beta_logito" = betareg(formula = WINP ~ TEAM + FGP + FGA + `3PP` + PF + PlusMinus, data = dados_cross),
  "beta_loglog" = betareg(formula = WINP ~ TEAM + PF + OREB + FGP + PlusMinus, data = dados_cross, link = "loglog"),
  "beta_probito" = betareg(formula = WINP ~ TEAM + PF + FGP + PlusMinus, data = dados_cross, link = "probit"),
  "beta_cloglog" = betareg(formula = WINP ~ TEAM + TOV + FGA + FGM + PlusMinus, data = dados_cross, link = "cloglog"),
  "beta_cauchit" = betareg(formula = WINP ~ PF + FGA + FGM + PlusMinus, data = dados_cross, link = "cauchit"),
  "gamlss_beta" = gamlss(formula = WINP ~ PlusMinus + FGP + PTS + PF + TEAM, family = BE, data = dados_cross),
  "misto_normal_team" = lmer(formula = WINP ~ PlusMinus + OREB + PF + TresPA + (1|TEAM), data = dados_cross),
  "misto_normal_temp" = lmer(formula = WINP ~ PlusMinus + FGP + PF + FGM + TEAM + (1|Numero_temporada), data = dados_cross),
  "misto_beta_team" = gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + FGP + PTS + PF, family = BE, data = dados_cross),
  "misto_beta_temp" = gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + PF + FGP + FGA, family = BE, data = dados_cross) 
)

# Definindo as métricas
metricas <- c("R2", "RMSE", "MAE")

# Lista para armazenar os resultados
# Você cria uma lista vazia chamada resultados para armazenar 
# os resultados das métricas para cada modelo.
resultados <- list()

# Loop para execução da validação cruzada
for (nome_modelo in names(modelos)) {
  for (m in metricas) {
    resultados[[paste(nome_modelo, m, sep = "_")]] <- c()
  }
}

# Executando a validação cruzada 5 vezes
# Você usa um loop para executar a validação cruzada. Para isso, você cria os 
# folds de validação cruzada usando a função createFolds e, em seguida, divide 
# os dados em conjuntos de treinamento e teste com base nesses folds.

for (i in 1:5) {
  # Criando os folds para validação cruzada
  folds <- createFolds(dados_cross$WINP, k = 5, returnTrain = TRUE)
  
  # Loop para cada fold
  for (j in 1:length(folds)) {
    # Dividindo os dados em treino e teste
    training_index <- folds[[j]]
    testing_index <- setdiff(seq_len(nrow(dados_cross)), training_index)
    training_data <- dados_cross[training_index, ]
    testing_data <- dados_cross[testing_index, ]
    
    # Treinando e testando cada modelo
    for (nome_modelo in names(modelos)) {
      modelo <- modelos[[nome_modelo]]
      predict_test <- predict(modelo, newdata = testing_data)
      for (m in metricas) {
        resultados[[paste(nome_modelo, m, sep = "_")]] <- c(resultados[[paste(nome_modelo, m, sep = "_")]], 
                                                            ifelse(m == "R2", R2(predict_test, testing_data$WINP),
                                                                   ifelse(m == "RMSE", RMSE(predict_test, testing_data$WINP),
                                                                          MAE(predict_test, testing_data$WINP))))
      }
    }
  }
}

# Calculando a média das métricas para cada modelo
#Após a validação cruzada, você calcula a média das métricas para cada modelo.
medias_resultados <- list()
for (nome_modelo in names(modelos)) {
  for (m in metricas) {
    medias_resultados[[paste(nome_modelo, m, sep = "_")]] <- mean(resultados[[paste(nome_modelo, m, sep = "_")]])
  }
}

# Exibindo as médias das métricas
print("Médias das métricas para cada modelo:")
print(medias_resultados)

data.frame(Modelo = c("Regressão Linear", "Betareg Logito",
           "Betareg loglog","Betareg probito", 
           "Betareg cloglog","Betareg Cauchit","Gamlss Beta",
           "Misto Normal TEAM","Misto Normal TEMPORADA","Misto Beta TEAM",
           "Misto Beta TEMPORADA"),
           R2 = c(medias_resultados[[1]], medias_resultados[[4]],medias_resultados[[7]],
                  medias_resultados[[10]], medias_resultados[[13]], medias_resultados[[16]],
                  medias_resultados[[19]], medias_resultados[[22]],medias_resultados[[25]],
                  medias_resultados[[28]], medias_resultados[[31]]),
       RMSE = c(medias_resultados[[2]], medias_resultados[[5]],medias_resultados[[8]],
                medias_resultados[[11]], medias_resultados[[14]], medias_resultados[[17]],
                medias_resultados[[20]], medias_resultados[[23]],medias_resultados[[26]],
                medias_resultados[[29]], medias_resultados[[32]]),
       MAE = c(medias_resultados[[3]], medias_resultados[[6]],medias_resultados[[9]],
               medias_resultados[[12]], medias_resultados[[15]], medias_resultados[[18]],
               medias_resultados[[21]], medias_resultados[[24]],medias_resultados[[27]],
               medias_resultados[[30]], medias_resultados[[33]])
)

#Outra forma de Cross Validation, mas não será utilizada 
#porque apenas faz oara uma amostra só, precisando desenvolver para mais vezes 
#para ser interessante utilizar.

##### Cross Validation com apenas uma amostra #####
# R program to implement validation set approach

# setting seed to generate a reproducible random sampling
set.seed(236292)

# creating training data as 80% of the dataset
random_sample <- createDataPartition(dados_cross$WINP, p = 0.8, list = FALSE)

# generating training dataset from the random_sample
training_dataset  <- dados_cross[random_sample, ]

# generating testing dataset from rows which are not included in random_sample
testing_dataset <- dados_cross[-random_sample, ]

# Building the model

# training the model by assigning sales column as target variable and rest other columns
# as independent variables
regressao_linear = lm(formula = WINP ~ TEAM + PF + FGP + FGM + PlusMinus, data = training_dataset)
beta_logito = betareg(formula = WINP ~ TEAM + FGP + FGA + `3PP` + PF + PlusMinus, data = training_dataset)
beta_loglog = betareg(formula = WINP ~ TEAM + PF + OREB + FGP + PlusMinus, data = training_dataset, link = "loglog")
beta_probito = betareg(formula = WINP ~ TEAM + PF + FGP + PlusMinus, data = training_dataset, link = "probit")
beta_cloglog = betareg(formula = WINP ~ TEAM + TOV + FGA + FGM + PlusMinus, data = training_dataset, link = "cloglog")
beta_cauchit = betareg(formula = WINP ~ PF + FGA + FGM + PlusMinus, data = training_dataset, link = "cauchit")
gamlss_beta = gamlss(formula = WINP ~ PlusMinus + FGP + PTS + PF + TEAM, family = BE, data = training_dataset)
misto_normal_team = lmer(formula = WINP ~ PlusMinus + OREB + PF + TresPA + (1|TEAM), data = training_dataset)
misto_normal_temp = lmer(formula = WINP ~ PlusMinus + FGP + PF + FGM + TEAM + (1|Numero_temporada), data = training_dataset)
misto_beta_team = gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + FGP + PTS + PF, family = BE, data = training_dataset)
misto_beta_temp = gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + PF + FGP + FGA, family = BE, data = training_dataset) 

# predicting the target variable
predictions1 <- predict(regressao_linear, newdata = testing_dataset)
predictions3 <- predict(beta_logito, newdata = testing_dataset)
predictions4 <- predict(beta_loglog, newdata = testing_dataset)
predictions5 <- predict(beta_probito, newdata = testing_dataset)
predictions6 <- predict(beta_cloglog, newdata = testing_dataset)
predictions7 <- predict(beta_cauchit, newdata = testing_dataset)
predictions8 <- predict(gamlss_beta, newdata = testing_dataset)
predictions9 <- predict(misto_normal_team, newdata = testing_dataset)
predictions10 <- predict(misto_normal_temp, newdata = testing_dataset)
predictions11 <- predict(misto_beta_team, newdata = testing_dataset)
predictions12 <- predict(misto_beta_temp, newdata = testing_dataset)

# computing model performance metrics
data.frame(Modelo = c("Regressão linear","betareg logito",
                      "betareg loglog","betareg probito", 
                      "betareg cloglog","Betareg Cauchit","Gamlss Beta",
                      "Misto Normal TEAM","Misto Normal TEMPORADA","Misto Beta TEAM",
                      "Misto Beta TEMPORADA"),
           R2 = c(R2(predictions1, testing_dataset$WINP),
                  R2(predictions3, testing_dataset$WINP),R2(predictions4, testing_dataset$WINP),
                  R2(predictions5, testing_dataset$WINP), R2(predictions6, testing_dataset$WINP),
                  R2(predictions7, testing_dataset$WINP), R2(predictions8, testing_dataset$WINP),
                  R2(predictions9, testing_dataset$WINP), R2(predictions10, testing_dataset$WINP),
                  R2(predictions11, testing_dataset$WINP), R2(predictions12, testing_dataset$WINP)),
           RMSE = c(RMSE(predictions1, testing_dataset$WINP), 
                    RMSE(predictions3, testing_dataset$WINP), RMSE(predictions4, testing_dataset$WINP),
                    RMSE(predictions5, testing_dataset$WINP), RMSE(predictions6, testing_dataset$WINP), 
                    RMSE(predictions7, testing_dataset$WINP), RMSE(predictions8, testing_dataset$WINP),
                    RMSE(predictions9, testing_dataset$WINP), RMSE(predictions10, testing_dataset$WINP), 
                    RMSE(predictions11, testing_dataset$WINP), RMSE(predictions12, testing_dataset$WINP)),
           MAE = c(MAE(predictions1, testing_dataset$WINP), 
                   MAE(predictions3, testing_dataset$WINP), MAE(predictions4, testing_dataset$WINP),
                   MAE(predictions5, testing_dataset$WINP), MAE(predictions6, testing_dataset$WINP),
                   MAE(predictions7, testing_dataset$WINP), MAE(predictions8, testing_dataset$WINP),
                   MAE(predictions9, testing_dataset$WINP), MAE(predictions10, testing_dataset$WINP),
                   MAE(predictions11, testing_dataset$WINP), MAE(predictions12, testing_dataset$WINP)))