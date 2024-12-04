source("dados_regular.R")

########## Regressão com todos os dados do modelo ###########
modelo1 <- lm(WINP ~ .,data = dados_regressao)
modelo1
coef(modelo1)
anova(modelo1)
summary(modelo1) #R^2_ajustado = 0.9365 
#Nesse modelo completo, apenas Plus_Minus, STL e PF foi significante com um alfa de
#5%, mas se alfa = 10%, teremos também FTM significantes.

########## Regressão com apenas  PlusMinus + STL + PF no modelo (significante 5%) #########
modelo2 <- lm(WINP ~  PlusMinus + STL + PF,data = dados_regressao) 
modelo2
coef(modelo2)
anova(modelo2) #STL não significante
summary(modelo2) #Adjusted R-squared:  0.9333, bem perto do que foi do modelo1

######## Regressão com apenas Plus_Minus, FTM, STL e PF no modelo (que foram significantes no modelo com alfa = 10%) ####
modelo3 <- lm(WINP ~ PlusMinus + STL + PF + FTM,data = dados_regressao) 
modelo3
coef(modelo3)
anova(modelo3)#STL e FTM não deu significante
summary(modelo3) #Adjusted R-squared:  0.9333 

######### Regressão com apenas Plus_Minus e PF no modelo ########
modelo4 <- lm(WINP ~ PlusMinus + PF,data = dados_regressao) 
modelo4
coef(modelo4)
anova(modelo4)
summary(modelo4) #Adjusted R-squared:  0.9334

########## backward regression ###########
#Selecão das variáveis para compor o modelo, mas precisa depois fazer os teste de resíduo
completo = lm(WINP ~ ., data = dados_regressao)
vazio = lm(WINP ~ 1, data = dados_regressao)
step(completo, scope=list(upper=completo, lower=vazio), direction='backward', trace=TRUE)

# lm(formula = WINP ~ TEAM + PTS + FGP + `3PM` + FTM + FTA + FTP + 
#OREB + DREB + TOV + STL + PFD + PlusMinus, data = dados_regressao)
modelo_back <- lm(formula = WINP ~ TEAM + PTS + FGP + `3PM` + FTM + FTA + FTP + 
                    OREB + DREB + TOV + STL + PFD + PlusMinus, data = dados_regressao)
modelo_back
coef(modelo_back)
anova(modelo_back)
summary(modelo_back) #Adjusted R-squared:  0.9344
AIC(modelo_back) #-1645.353

######## Forward Selection ########
completo = lm(WINP ~ ., data = dados_regressao)
vazio = lm(WINP ~ 1, data = dados_regressao)
step(vazio, scope=list(upper=completo, lower=vazio), direction='forward', trace=TRUE)
# Coefficients:
#   (Intercept)   Plus_Minus           PF         FG_P          FGM  
# 0.401565     0.030261    -0.003478     0.005746    -0.002433

modelo_forw <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM, data = dados_regressao)
modelo_forw
coef(modelo_forw)
anova(modelo_forw)
summary(modelo_forw) #Adjusted R-squared:  0.9345 

############## Análise de resíduos ############
##### Modelo completo #####
plot(modelo1, which = 1)
plot(modelo1, which = 2)
plot(modelo1, which = 3)
plot(modelo1, which = 4)
plot(modelo1, which = 5)
plot(modelo1, which = 6)
shapiro.test(modelo1$residuals) #p-value = 0.1885, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo1) #p-value = 0.1306
#Independência
plot(modelo1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo1$fitted.values, modelo1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo1) #p-value = 0.2463, heterocedasticidade
#QQ Plot
library(hnp)
hnp(modelo1)
AIC(modelo1)

####### Modelo 2 #####
plot(modelo2, which = 1)
plot(modelo2, which = 2)
plot(modelo2, which = 3)
plot(modelo2, which = 4)
plot(modelo2, which = 5)
plot(modelo2, which = 6)
shapiro.test(modelo2$residuals) #p-value = 0.5054, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo2) #p-value = 0.2889
#Independência
plot(modelo2$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo2$fitted.values, modelo2$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo2) #p-value = 0.03674, heterocedasticidade
#QQ Plot
library(hnp)
hnp(modelo2)

###### Modelo 3 ########
plot(modelo3, which = 1)
plot(modelo3, which = 2)
plot(modelo3, which = 3)
plot(modelo3, which = 4)
plot(modelo3, which = 5)
plot(modelo3, which = 6)
shapiro.test(modelo3$residuals) #p-value = 0.1847, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo3) #p-value = 0.2497
#Independência
plot(modelo3$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo3$fitted.values, modelo3$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo3) #p-value = 0.001367, heterocedasticidade
#QQ Plot
library(hnp)
hnp(modelo3)


##### Modelo 4 ######
plot(modelo4, which = 1)
plot(modelo4, which = 2)
plot(modelo4, which = 3)
plot(modelo4, which = 4)
plot(modelo4, which = 5)
plot(modelo4, which = 6)
shapiro.test(modelo4$residuals) #p-value = 0.1829, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo4) #p-value = 0.254
#Independência
plot(modelo4$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo4$fitted.values, modelo4$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo4) #p-value = 0.0008406, heterocedasticidade
#QQ Plot
library(hnp)
hnp(modelo4)


####### Backward ###########
plot(modelo_back, which = 1)
plot(modelo_back, which = 2)
plot(modelo_back, which = 3)
plot(modelo_back, which = 4)
plot(modelo_back, which = 5)
plot(modelo_back, which = 6)
shapiro.test(modelo_back$residuals) #p-value = 0.2669, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_back) #p-value = 0.1735
#Independência
plot(modelo_back$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_back$fitted.values, modelo_back$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_back) #p-value = 0.0006407, heterocedasticidade
#QQ Plot
library(hnp)
hnp(modelo_back)


######## Forward ##########
plot(modelo_forw, which = 1)
plot(modelo_forw, which = 2) #QQ-plot
plot(modelo_forw, which = 3)
plot(modelo_forw, which = 4)
plot(modelo_forw, which = 5)
plot(modelo_forw, which = 6)
shapiro.test(modelo_forw$residuals) #p-value = 0.2296, não normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_forw) #p-value = 0.195
#Independência
plot(modelo_forw$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_forw$fitted.values, modelo_forw$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_forw) #p-value = 0.001575, heterocedasticidade
library(hnp)
hnp(modelo_forw)



################## Análise de Anova ##############
modelo1 #Completo
modelo2 #PlusMinus, STL, PF 
modelo3 #PlusMinus + STL + PF + FTM 
modelo4 #PlusMinus, PF  
modelo_back #TEAM + PTS + FGP + `3PM` + FTM + FTA + FTP + OREB + DREB + TOV + STL + PFD + PlusMinus
modelo_forw #PlusMinus + PF + FGP + FGM
modelo_nada <- lm(formula = WINP ~ 1, data = dados_regressao)
modelo_plus <- lm(formula = WINP ~ PlusMinus, data = dados_regressao)
modelo_fgp <- lm(formula = WINP ~ PlusMinus + PF + FGP, data = dados_regressao)
modelo_forw <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM, data = dados_regressao)
modelo_pfd <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + PFD, data = dados_regressao)
modelo_stl <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + STL, data = dados_regressao)
modelo_tov <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + TOV, data = dados_regressao)
modelo_dreb <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + DREB, data = dados_regressao)
modelo_oreb <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + OREB, data = dados_regressao)
modelo_oreb <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + OREB, data = dados_regressao)
modelo_ftp <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + FTP, data = dados_regressao)
modelo_fta <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + FTA, data = dados_regressao)
modelo_ftm <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + FTM, data = dados_regressao)
modelo_3pm <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + `3PM`, data = dados_regressao)
modelo_pts <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + PTS, data = dados_regressao)
modelo_team <- lm(formula = WINP ~ PlusMinus + PF + FGP + FGM + TEAM, data = dados_regressao)

anova(modelo_nada, modelo_plus) #PlusMinus significante
anova(modelo_plus, modelo4) #PF significante
anova(modelo4, modelo_fgp) #FGP significante
anova(modelo_fgp, modelo_forw) #FGM significante
anova(modelo_forw, modelo_pfd) #0.7838, PFD não significante
anova(modelo_forw, modelo_stl) #0.5605, STL não significante
anova(modelo_forw, modelo_tov) #0.566, TOV não significante
anova(modelo_forw, modelo_dreb) #0.2455, DREB não significante
anova(modelo_forw, modelo_oreb) #0.3187, OREB não significante
anova(modelo_forw, modelo_ftp) #0.9444, FTP não significante
anova(modelo_forw, modelo_fta) #0.7826, FTA não significante
anova(modelo_forw, modelo_ftm) #0.804, FTM não significante
anova(modelo_forw, modelo_3pm) #0.7518, 3PM não significante
anova(modelo_forw, modelo_pts) #0.9504, PTS não significante
anova(modelo_forw, modelo_team) #0.009456, TEAM significante

############# Conclusão ############
#Dos modelos testados modelo_forw foi o melhor encontrado,
#precisando fazer uma análise mais aprofundada.
#Ele contém  PlusMinus + PF + FGP + FGM + TEAM