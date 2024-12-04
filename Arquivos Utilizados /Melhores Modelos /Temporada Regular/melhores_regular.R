source("dados_regular.R")

########## Regressão linear ########
regressao_linear <- lm(WINP ~ TEAM + PF + FGP + FGM + PlusMinus, data = dados_regressao)
regressao_linear
coef(regressao_linear)
anova(regressao_linear)
summary(regressao_linear) #Adjusted R-squared:  0.9378
AIC(regressao_linear) #-1638.956
###Resíduos ###
plot(regressao_linear, which = 1)
plot(regressao_linear, which = 2)
plot(regressao_linear, which = 3)
plot(regressao_linear, which = 4)
plot(regressao_linear, which = 5)
plot(regressao_linear, which = 6)
shapiro.test(regressao_linear$residuals) #p-value = 0.3702, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(regressao_linear) #p-value = 0.2728
#Independência
plot(regressao_linear$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(regressao_linear$fitted.values, regressao_linear$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(regressao_linear) #p-value = 0.1371, homocedasticidade
#QQ Plot
library(hnp)
hnp(regressao_linear)

########## Regressão beta ########
######## Logito ##########
#Melhor modelo logito é o modelo com `3PP` + PF + PlusMinus que é modelo_beta12_3.
beta_logito <- betareg(WINP ~ TEAM + FGP + FGA + `3PP` + PF + PlusMinus, data = dados_regressao)
beta_logito
summary(beta_logito) #Pseudo R-squared: 0.9398
coef(beta_logito)
car::Anova(beta_logito) #PF e 3PP não significante
########Resíduos Logito ###
plot(beta_logito, which = 1, type = "pearson")
plot(beta_logito, which = 2, type = "pearson")
plot(beta_logito, which = 3, type = "pearson")
plot(beta_logito, which = 4, type = "pearson")
plot(beta_logito, which = 5, type = "deviance", sub.caption = "")
#plot(beta_logito, which = 1, type = "deviance", sub.caption = "")
shapiro.test(beta_logito$residuals) #p-value = 0.1817, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(beta_logito) #p-value = 0.2521
#Independência
plot(beta_logito$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(beta_logito$fitted.values, beta_logito$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(beta_logito) #p-value = 0.1002, heterocedasticidade

######## Loglog ##########
#Melhor modelo de loglog é o modelo modelo_beta21 com STL + PF + PlusMinus;
beta_loglog <- betareg(WINP ~ TEAM + PF + OREB + FGP + PlusMinus,data = dados_regressao, link = "loglog") #Regressão com todos os dados do modelo
beta_loglog
summary(beta_loglog) #Pseudo R-squared: 0.9315
coef(beta_loglog)
car::Anova(beta_loglog) # OREB e FGP não significativo
#### Resíduos loglog ##
plot(beta_loglog, which = 1, type = "pearson")
plot(beta_loglog, which = 2, type = "pearson")
plot(beta_loglog, which = 3, type = "pearson")
plot(beta_loglog, which = 4, type = "pearson")
#plot(beta_loglog, which = 5, type = "deviance", sub.caption = "")
#plot(beta_loglog, which = 1, type = "deviance", sub.caption = "")
shapiro.test(beta_loglog$residuals) #p-value = 0.5431
#Teste de durbin watson para independencia
library(lmtest)
dwtest(beta_loglog) #p-value = 0.2217
#Independência
plot(beta_loglog$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(beta_loglog$fitted.values, beta_loglog$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(beta_loglog) #p-value = 0.09734
######## Probito ##########
#Melhor modelo de probito é modelo_beta_probit2 com `3PP` + TOV + STL + PF + PlusMinus;
beta_probito <- betareg(WINP ~ TEAM + PF + FGP + PlusMinus,data = dados_regressao, link = "probit")
beta_probito
summary(beta_probito) #Pseudo R-squared: 0.9331
coef(beta_probito)
car::Anova(beta_probito) #PF não significativo
### Resíduos Probito ###
plot(beta_probito, which = 1, type = "pearson")
plot(beta_probito, which = 2, type = "pearson")
plot(beta_probito, which = 3, type = "pearson")
plot(beta_probito, which = 4, type = "pearson")
#plot(beta_probito, which = 5, type = "deviance", sub.caption = "")
#plot(beta_probito, which = 1, type = "deviance", sub.caption = "")
shapiro.test(beta_probito$residuals) #p-value = 0.2916
#Teste de durbin watson para independencia
library(lmtest)
dwtest(beta_probito) #p-value = 0.2491
#Independência
plot(beta_probito$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(beta_probito$fitted.values, beta_probito$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(beta_probito) #p-value =  0.1038
######## cloglog ##########
#melhor modelo é modelo_beta_cloglog_1 com TOV + PlusMinus
beta_cloglog <- betareg(WINP ~ TEAM + TOV + FGA + FGM + PlusMinus,data = dados_regressao, link = "cloglog")
beta_cloglog
summary(beta_cloglog) #Pseudo R-squared: 0.934
coef(beta_cloglog)
car::Anova(beta_cloglog) #Todos significantes
#Resíduos cloglog
plot(beta_cloglog, which = 1, type = "pearson")
plot(beta_cloglog, which = 2, type = "pearson")
plot(beta_cloglog, which = 3, type = "pearson")
plot(beta_cloglog, which = 4, type = "pearson")
#plot(beta_cloglog, which = 5, type = "deviance", sub.caption = "")
#plot(beta_cloglog, which = 1, type = "deviance", sub.caption = "")
shapiro.test(beta_cloglog$residuals) #p-value = 0.3723
#Teste de durbin watson para independencia
library(lmtest)
dwtest(beta_cloglog) #p-value = 0.1396
#Independência
plot(beta_cloglog$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(beta_cloglog$fitted.values, beta_cloglog$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(beta_cloglog) #p-value = 0.2768

######## cauchito ##########
beta_cauchit <- betareg(WINP ~ PF + FGA + FGM + PlusMinus,data = dados_regressao, link = "cauchit")
beta_cauchit
summary(beta_cauchit) #Pseudo R-squared: 0.8999
coef(beta_cauchit)
car::Anova(beta_cauchit) #Todos significantes
#Resíduos
plot(beta_cauchit, which = 1, type = "pearson")
plot(beta_cauchit, which = 2, type = "pearson")
plot(beta_cauchit, which = 3, type = "pearson")
plot(beta_cauchit, which = 4, type = "pearson")
#plot(beta_cauchit, which = 5, type = "deviance", sub.caption = "")
#plot(beta_cauchit, which = 1, type = "deviance", sub.caption = "")
shapiro.test(beta_cauchit$residuals) #p-value = 0.1809
#Teste de durbin watson para independencia
library(lmtest)
dwtest(beta_cauchit) #p-value = 0.1847
#Independência
plot(beta_cauchit$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(beta_cauchit$fitted.values, beta_cauchit$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(beta_cauchit) #p-value = 0.001441
########## GAMLSS ########  
####### Beta #####
gamlss_beta <- gamlss(formula = WINP ~ PlusMinus + FGP + PTS + PF + TEAM, family = BE, data = dados_regressao)
gamlss_beta
coef(gamlss_beta)
summary(gamlss_beta) #PF não significativo

##### Resíduos ###
plot(gamlss_beta)
shapiro.test(gamlss_beta$residuals) #p-value = 0.1865, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_beta) #p-value = 0.242
#Independência
plot(gamlss_beta$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_beta) #p-value = 0.08981

######## Normal ##########
#Mesma que a linear então não iremos utilizar
gamlss_normal <- gamlss(formula = WINP ~ PlusMinus + PF + FGP + FGM + TEAM, family = NO, data = dados_regressao) 
gamlss_normal
coef(gamlss_normal)
summary(gamlss_normal) #-1646.44
#Resíduos forw
plot(gamlss_normal)
shapiro.test(gamlss_normal$residuals) #p-value = 0.2296, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_normal) #p-value =  0.195
#Independência
plot(gamlss_normal$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_normal) #p-value = 0.001575

########## Modelos Mistos ######## 
##### Normal TEAM #####
library(lme4)
misto_normal_team <- lmer(formula = WINP ~ PlusMinus + OREB + PF + `3PA` + (1|TEAM), data = dados_regressao)
misto_normal_team
coef(misto_normal_team)
sort(unique(dados_regular$TEAM))
dados_efeitos <- data.frame(TEAM = sort(unique(dados_regular$TEAM)),
                            Efeito_aleatorio = coef(misto_normal_team)$TEAM$`(Intercept)`)
dados_efeitos <- dados_efeitos[order(dados_efeitos$Efeito_aleatorio), ]
# Criar o gráfico de linha
dados_efeitos %>% filter(TEAM != "Charlotte Bobcats", TEAM != "New Orleans Hornets", TEAM != "New Jersey Nets") %>% 
  ggplot(aes(x = seq_along(TEAM), y = Efeito_aleatorio, color = factor(TEAM, levels = dados_efeitos$TEAM))) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(Efeito_aleatorio, 3)), vjust = -0.5, hjust = -0.5, size = 3) +  # Adicionar os valores dos efeitos aleatórios
  labs(x = "Posição", y = "Efeito Aleatório") +
  theme_minimal() +
  theme(legend.text = element_text(size = 10),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))  # Aumentar o tamanho do texto da legenda

summary(misto_normal_team) #AIC:
anova(misto_normal_team)
AIC(misto_normal_team)
library(GGally)
ggpairs(dados_regular %>% dplyr::select(WINP, PlusMinus, OREB, PF, `3PA`))
#### Análise de resíduos ###

# Extrair os resíduos
residuos <- resid(misto_normal_team)

# Calcular os resíduos padronizados
residuos_padronizados <- resid(misto_normal_team, type = "pearson")

# Extrair os valores ajustados
valores_ajustados <- fitted(misto_normal_team)

# Criar um data frame com os valores ajustados e os resíduos padronizados
data <- data.frame(Valores_Ajustados = valores_ajustados, Residuos_Padronizados = residuos_padronizados)

## Histograma dos residuos
ggplot(data.frame(Residuos = residuos), aes(x = Residuos)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue", alpha = 0.6) +
  geom_density(color = "blue") + # Adiciona a curva de densidade
  theme_bw()+
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"))

#Boxplot dos residuos
ggplot(data.frame(residuos)) + 
  geom_boxplot(aes(x = residuos), fill = "orange") + theme_bw()+
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"))

# Teste de normalidade dos resíduos
shapiro.test(residuos)

#QQ Plot
library(hnp)
hnp(misto_normal_team, halfnormal = F)

## Gráfico de valores ajustados x resíduos (homocedasticidade)
plot(misto_normal_team,type=c("p","smooth")) 

# Calcular os limites superior e inferior para identificação de outliers
limite_superior <- quantile(residuos_padronizados, 0.975)
limite_inferior <- quantile(residuos_padronizados, 0.025)

# Identificar os outliers
outliers <- data[data$Residuos_Padronizados < limite_inferior | data$Residuos_Padronizados > limite_superior, ]

# Criar o gráfico de dispersão com identificação de outliers
plot(data$Valores_Ajustados, data$Residuos_Padronizados,
     xlab = "Valores Ajustados",
     ylab = "Resíduos Padronizados",
     main = "Diagrama de Dispersão de Valores Ajustados vs. Resíduos Padronizados",
     pch = ifelse(data$Residuos_Padronizados < limite_inferior | data$Residuos_Padronizados > limite_superior, 19, 16), 
     col = ifelse(data$Residuos_Padronizados < limite_inferior | data$Residuos_Padronizados > limite_superior, "red", "black"))

# Adicionar uma linha horizontal em y = 0
abline(h = 0, col = "gray")

# Adicionar linha de identificação dos limites de outliers
abline(h = limite_superior, col = "red", lwd = 2)
abline(h = limite_inferior, col = "red", lwd = 2)


#Outras análises de resíduos que não foram utilizadas
##gráfico quantil-quantil (normalidade)
qqmath(misto_normal_team,id=0.05) 
# Criar o QQ plot
qqnorm(residuos)
qqline(residuos, col = "blue") # Adiciona uma linha de referência em azul

library(RVAideMemoire)
plotresid(misto_normal_team, shapiro = T)

# Histograma dos resíduos padronizados
hist(residuos_padronizados,
     main = "Histograma dos Resíduos Padronizados",
     xlab = "Resíduos Padronizados",
     ylab = "Frequência")

# Gráfico de valores ajustados versus resíduos
plot(fitted(misto_normal_team), residuos,
     xlab = "Valores Ajustados",
     ylab = "Resíduos",
     main = "Gráfico de Valores Ajustados vs. Resíduos")

# Adicionar uma linha horizontal em y = 0
abline(h = 0, col = "red")

# Gráfico de valores ajustados versus resíduos padronizados
plot(fitted(misto_normal_team), residuos_padronizados,
     xlab = "Valores Ajustados",
     ylab = "Resíduos Padronizados",
     main = "Gráfico de Valores Ajustados vs. Resíduos Padronizados")

# Adicionar uma linha horizontal em y = 0
abline(h = 0, col = "red")

#Resíduos
plot(misto_normal_team)

##Gráfico de valores ajustados x resíduos padronizados (homocedasticidade)
plot(misto_normal_team,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))

######Normal Temporada #####
library(lme4)
misto_normal_temp <- lmer(formula = WINP ~ PlusMinus + FGP + PF + FGM + TEAM + (1|Numero_temporada), data = dados_regressao)
misto_normal_temp
coef(misto_normal_temp)
summary(misto_normal_temp) #AIC:
anova(misto_normal_temp)
AIC(misto_normal_temp)

###### Beta Team ####
misto_beta_team <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) +  
                            PlusMinus + FGP + PTS + PF, family = BE, data = dados_regressao) 
misto_beta_team
coef(misto_beta_team)
summary(misto_beta_team) #AIC:
getSmo(misto_beta_team)
#Resíduos
plot(misto_beta_team)
shapiro.test(misto_beta_team$residuals) #p-value =  normal
#Independência
plot(misto_beta_team$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(misto_beta_team) #p-value = 

#####  Beta Temporada #####
misto_beta_temp <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) +  
         PlusMinus + PF + FGP + FGA, family = BE, data = dados_regressao) 
misto_beta_temp
coef(misto_beta_temp)
summary(misto_beta_temp) #AIC:
getSmo(misto_beta_temp)
#Resíduos
plot(misto_beta_temp)
shapiro.test(misto_beta_temp$residuals) #p-value =  normal
#Independência
plot(misto_beta_temp$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(misto_beta_temp) #p-value = 