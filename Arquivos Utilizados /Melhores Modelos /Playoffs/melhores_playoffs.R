source("dados_playoffs.R")
#Vamos mudar o nome porque estava dando erro na hora do gamlss
dados_regressaop$WINP_transformado <- (dados_regressaop$WINP*(240 - 1) + 0.5)/240
########## Regressão linear ########
regressao_linearp <- lm(WINP ~ TEAM + DREB + PlusMinus, data = dados_regressaop)
regressao_linearp
coef(regressao_linearp)
anova(regressao_linearp)
summary(regressao_linearp) #Adjusted R-squared:  0.7729
AIC(regressao_linearp) #-1645.353
###Resíduos ###
hnp(regressao_linearp, halfnormal = F)
plot(regressao_linearp, which = 1)
plot(regressao_linearp, which = 2)
plot(regressao_linearp, which = 3)
plot(regressao_linearp, which = 4)
plot(regressao_linearp, which = 5)
plot(regressao_linearp, which = 6)
shapiro.test(regressao_linearp$residuals) #p-value = 0.006085, não normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(regressao_linearp) #p-value = 0.06677
#Independência
plot(regressao_linearp$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(regressao_linearp$fitted.values, regressao_linearp$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(regressao_linearp) #p-value = 0.003522, heterocedasticidade
#QQ Plot
library(hnp)
hnp(regressao_linearp)

########## Regressão beta ########
######## Logito ##########
#Melhor modelo logito é o modelo com `3PP` + PF + PlusMinus que é modelo_beta12_3.
beta_logitop <- betareg(WINP_transformado ~ TEAM + FTP + REB + PlusMinus, data = dados_regressaop)
beta_logitop
#summary(beta_logitop) 
coef(beta_logitop)
car::Anova(beta_logitop) ##FTP e REB não significativas
########Resíduos Logito ###
plot(beta_logitop, which = 1, type = "pearson")
plot(beta_logitop, which = 2, type = "pearson")
plot(beta_logitop, which = 3, type = "pearson")
plot(beta_logitop, which = 4, type = "pearson")
plot(beta_logitop, which = 5, type = "deviance", sub.caption = "")
plot(beta_logitop, which = 1, type = "deviance", sub.caption = "")
shapiro.test(beta_logitop$residuals) #p-value = 0.2462, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(beta_logitop) #p-value = 0.04831
#Independência
plot(beta_logitop$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(beta_logitop$fitted.values, beta_logitop$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(beta_logitop) #p-value = 0.007814, heterocedasticidade

######## Loglog ##########
#Melhor modelo de loglog é o modelo modelo_beta21 com STL + PF + PlusMinus;
beta_loglogp <- betareg(WINP_transformado ~ TEAM + REB + PlusMinus,data = dados_regressaop, link = "loglog") #Regressão com todos os dados do modelo
beta_loglogp
#summary(beta_loglogp) 
coef(beta_loglogp)
car::Anova(beta_loglogp) #Todas as variáveis significativas
#### Resíduos loglog ##
plot(beta_loglogp, which = 1, type = "pearson")
plot(beta_loglogp, which = 2, type = "pearson")
plot(beta_loglogp, which = 3, type = "pearson")
plot(beta_loglogp, which = 4, type = "pearson")
plot(beta_loglogp, which = 5, type = "deviance", sub.caption = "")
plot(beta_loglogp, which = 1, type = "deviance", sub.caption = "")
shapiro.test(beta_loglogp$residuals) #p-value = 0.4186
#Teste de durbin watson para independencia
library(lmtest)
dwtest(beta_loglogp) #p-value = 0.06218
#Independência
plot(beta_loglogp$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(beta_loglogp$fitted.values, beta_loglogp$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(beta_loglogp) #p-value = 0.007102
######## Probito ##########
#Melhor modelo de probito é modelo_beta_probit2 com `3PP` + TOV + STL + PF + PlusMinus;
beta_probitop <- betareg(WINP_transformado ~ TEAM + FTP + REB + PlusMinus,data = dados_regressaop, link = "probit")
beta_probitop
# summary(beta_probitop) 
coef(beta_probitop)
car::Anova(beta_probitop) #FTP e REB não significativas
### Resíduos Probito ###
plot(beta_probitop, which = 1, type = "pearson")
plot(beta_probitop, which = 2, type = "pearson")
plot(beta_probitop, which = 3, type = "pearson")
plot(beta_probitop, which = 4, type = "pearson")
plot(beta_probitop, which = 5, type = "deviance", sub.caption = "")
plot(beta_probitop, which = 1, type = "deviance", sub.caption = "")
shapiro.test(beta_probitop$residuals) #p-value = 0.2427
#Teste de durbin watson para independencia
library(lmtest)
dwtest(beta_probitop) #p-value = 0.04831
#Independência
plot(beta_probitop$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(beta_probitop$fitted.values, beta_probitop$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(beta_probitop) #p-value = 0.007814
######## cloglog ##########
beta_cloglogp <- betareg(WINP_transformado ~ TEAM + FTP + REB + PlusMinus,data = dados_regressaop, link = "cloglog")
beta_cloglogp
#summary(beta_cloglogp) 
coef(beta_cloglogp)
car::Anova(beta_cloglogp) #FTP e REB não significativas
#Resíduos cloglog
plot(beta_cloglogp, which = 1, type = "pearson")
plot(beta_cloglogp, which = 2, type = "pearson")
plot(beta_cloglogp, which = 3, type = "pearson")
plot(beta_cloglogp, which = 4, type = "pearson")
plot(beta_cloglogp, which = 5, type = "deviance", sub.caption = "")
plot(beta_cloglogp, which = 1, type = "deviance", sub.caption = "")
shapiro.test(beta_cloglogp$residuals) #p-value = 0.2445
#Teste de durbin watson para independencia
library(lmtest)
dwtest(beta_cloglogp) #p-value = 0.04831
#Independência
plot(beta_cloglogp$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(beta_cloglogp$fitted.values, beta_cloglogp$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(beta_cloglogp) #p-value = 0.007814

######## cauchito ##########
beta_cauchitp <- betareg(WINP_transformado ~ TEAM + FTP + PlusMinus, data = dados_regressaop, link = "cauchit")
beta_cauchitp
#summary(beta_cauchitp) 
coef(beta_cauchitp)
car::Anova(beta_cauchitp) #TEAM e FTP n~so significativo
#Resíduos
plot(beta_cauchitp, which = 1, type = "pearson")
plot(beta_cauchitp, which = 2, type = "pearson")
plot(beta_cauchitp, which = 3, type = "pearson")
plot(beta_cauchitp, which = 4, type = "pearson")
plot(beta_cauchitp, which = 5, type = "deviance", sub.caption = "")
plot(beta_cauchitp, which = 1, type = "deviance", sub.caption = "")
shapiro.test(beta_cauchitp$residuals) #p-value = 0.6223
#Teste de durbin watson para independencia
library(lmtest)
dwtest(beta_cauchitp) #p-value = 0.04703
#Independência
plot(beta_cauchitp$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(beta_cauchitp$fitted.values, beta_cauchitp$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(beta_cauchitp) #p-value = 0.004448
########## GAMLSS ########  
####### Beta #####
gamlss_betap <- gamlss(formula = WINP ~ PlusMinus + PF + TEAM, family = BEZI, data = dados_regressaop)
gamlss_betap
coef(gamlss_betap)
summary(gamlss_betap) 
##### Resíduos ###
plot(gamlss_betap)
shapiro.test(gamlss_betap$residuals) #p-value = 5.195e-05 não normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_betap) #p-value = 0.07946
#Independência
plot(gamlss_betap$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_betap) #p-value = 0.002333

######## Normal ##########
#Mesma que a linear então não iremos utilizar
gamlss_normalp <- gamlss(formula = WINP ~ PlusMinus + DREB + TEAM, family = NO, data = dados_regressaop) 
gamlss_normalp
coef(gamlss_normalp)
summary(gamlss_normalp) 
#Resíduos forw
plot(gamlss_normalp)
shapiro.test(gamlss_normalp$residuals) #p-value = 0.006085 não normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_normalp) #p-value =  0.06677
#Independência
plot(gamlss_normalp$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_normalp) #p-value = 0.003522

########## Modelos Mistos ######## 
##### Normal TEAM #####
library(lme4)
misto_normal_teamp <- lmer(formula = WINP ~ DREB + PlusMinus + (1|TEAM), data = dados_regressaop)
misto_normal_teamp
coef(misto_normal_teamp)
summary(misto_normal_teamp) #AIC:
anova(misto_normal_teamp)
AIC(misto_normal_teamp)
#### Análise de resíduos ###

# Extrair os resíduos
residuosp <- resid(misto_normal_teamp)

# Calcular os resíduos padronizados
residuos_padronizadosp <- resid(misto_normal_teamp, type = "pearson")

# Extrair os valores ajustados
valores_ajustadosp <- fitted(misto_normal_teamp)

# Criar um data frame com os valores ajustados e os resíduos padronizados
datap <- data.frame(Valores_Ajustadosp = valores_ajustadosp, Residuos_Padronizadosp = residuos_padronizadosp)

## Histograma dos residuos
ggplot(data.frame(Residuos = residuosp), aes(x = Residuos)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +
  geom_density(color = "blue") + # Adiciona a curva de densidade
  theme_bw()

#Boxplot dos residuos
ggplot(data.frame(residuosp)) + 
  geom_boxplot(aes(x = residuosp), fill = "green") + theme_bw()

# Teste de normalidade dos resíduos
shapiro.test(residuosp) #0.0002561 não normal

# Criar o QQ plot
qqnorm(residuosp)
qqline(residuosp, col = "blue") # Adiciona uma linha de referência em azul

## Gráfico de valores ajustados x resíduos (homocedasticidade)
plot(misto_normal_teamp,type=c("p","smooth")) 

# Calcular os limites superior e inferior para identificação de outliers
limite_superiorp <- quantile(residuos_padronizadosp, 0.975)
limite_inferiorp <- quantile(residuos_padronizadosp, 0.025)

# Identificar os outliers
outliersp <- datap[datap$Residuos_Padronizadosp < limite_inferiorp | datap$Residuos_Padronizadosp > limite_superiorp, ]

# Criar o gráfico de dispersão com identificação de outliers
plot(datap$Valores_Ajustadosp, datap$Residuos_Padronizadosp,
     xlab = "Valores Ajustados",
     ylab = "Resíduos Padronizados",
     main = "Diagrama de Dispersão de Valores Ajustados vs. Resíduos Padronizados",
     pch = ifelse(datap$Residuos_Padronizadosp < limite_inferiorp | datap$Residuos_Padronizadosp > limite_superiorp, 19, 16), 
     col = ifelse(datap$Residuos_Padronizadosp < limite_inferiorp | datap$Residuos_Padronizadosp > limite_superiorp, "red", "black"))

# Adicionar uma linha horizontal em y = 0
abline(h = 0, col = "gray")

# Adicionar linha de identificação dos limites de outliers
abline(h = limite_superiorp, col = "red", lwd = 2)
abline(h = limite_inferiorp, col = "red", lwd = 2)


#Outras análises de resíduos que não foram utilizadas
##gráfico quantil-quantil (normalidade)
qqmath(misto_normal_teamp,id=0.05) 
#QQ Plot
library(hnp)
hnp(misto_normal_teamp, halfnormal = F)

library(RVAideMemoire)
plotresid(misto_normal_teamp, shapiro = T)

# Histograma dos resíduos padronizados
hist(residuos_padronizadosp,
     main = "Histograma dos Resíduos Padronizados",
     xlab = "Resíduos Padronizados",
     ylab = "Frequência")

# Gráfico de valores ajustados versus resíduos
plot(fitted(misto_normal_teamp), residuosp,
     xlab = "Valores Ajustados",
     ylab = "Resíduos",
     main = "Gráfico de Valores Ajustados vs. Resíduos")

# Adicionar uma linha horizontal em y = 0
abline(h = 0, col = "red")

# Gráfico de valores ajustados versus resíduos padronizados
plot(fitted(misto_normal_teamp), residuos_padronizadosp,
     xlab = "Valores Ajustados",
     ylab = "Resíduos Padronizados",
     main = "Gráfico de Valores Ajustados vs. Resíduos Padronizados")

# Adicionar uma linha horizontal em y = 0
abline(h = 0, col = "red")

#Resíduos
plot(misto_normal_teamp)

##Gráfico de valores ajustados x resíduos padronizados (homocedasticidade)
plot(misto_normal_teamp,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))

######Normal Temporada #####
library(lme4)
misto_normal_tempp <- lmer(formula = WINP ~ PlusMinus + DREB + TEAM + (1|Numero_temporada), data = dados_regressaop)
misto_normal_tempp
coef(misto_normal_tempp)
summary(misto_normal_tempp) #AIC:
anova(misto_normal_tempp)
AIC(misto_normal_tempp)
#Resíduos
plot(misto_normal_tempp)
# Extrair os resíduos
residuosp2 <- resid(misto_normal_tempp)
# Teste de normalidade dos resíduos
shapiro.test(residuosp2) #0.006085 não normal
###Resíduos ###
## 1) gráfico quantil-quantil (normalidade)
qqmath(misto_normal_tempp,id=0.05) 
#QQ Plot
library(hnp)
hnp(misto_normal_tempp)
## 2) gráfico de valores ajustados x resíduos (homocedasticidade)
plot(misto_normal_tempp,type=c("p","smooth")) 
## 3) gráfico de valores ajustados x resíduos padronizados (homocedasticidade)
plot(misto_normal_tempp,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))

library(RVAideMemoire)
plotresid(misto_normal_tempp, shapiro = T)

###### Beta Team ####
misto_beta_teamp <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) +  
                            PlusMinus + FTP + BLKA + PF, family = BEZI, data = dados_regressaop) 
misto_beta_teamp
coef(misto_beta_teamp)
summary(misto_beta_teamp) #AIC:
getSmo(misto_beta_teamp)
#Resíduos
plot(misto_beta_teamp)
shapiro.test(misto_beta_teamp$residuals) #p-value = 8.298e-06 não normal
#Independência
plot(misto_beta_teamp$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(misto_beta_teamp) #p-value = 0.0005133

#####  Beta Temporada #####
misto_beta_tempp <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) +  
                            PlusMinus + PF + TEAM, family = BEZI, data = dados_regressaop) 
misto_beta_tempp
coef(misto_beta_tempp)
summary(misto_beta_tempp) #AIC:
getSmo(misto_beta_tempp)
#Resíduos
plot(misto_beta_tempp)
shapiro.test(misto_beta_tempp$residuals) #p-value =  6.433e-05 não normal
#Independência
plot(misto_beta_tempp$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(misto_beta_tempp) #p-value = 0.002333