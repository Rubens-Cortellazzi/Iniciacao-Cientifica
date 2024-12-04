source("dados_playoffs.R")

##### Regressão com todos os dados do modelo #########
modelop1 <- lm(WINP ~ .,data = dados_regressaop) 
modelop1
coef(modelop1)
anova(modelop1)
summary(modelop1)

##### Regressão com as variáveis que foram significaivas com alfa = 10% ######
modelop4 <- lm(WINP ~ PTS + FGM + FGP + `3PM` + FTM + PF + PlusMinus,data = dados_regressaop) 
modelop4
coef(modelop4)
anova(modelop4) 
summary(modelop4) #Adjusted R-squared: 0.74856 e FGP não deu significante  

#### Regressão com as variáveis que foram significaivas com alfa = 10% sem FGP ####
modelop5 <- lm(WINP ~ PTS + FGM + `3PM` + FTM + PF + PlusMinus,data = dados_regressaop) 
modelop5
coef(modelop5)
anova(modelop5) 
summary(modelop5) #Adjusted R-squared: 0.7468  e os que deram não significantes foram os que não estavam significantes com 10%.

###### backward selection #####
#Selecão das variáveis para compor o modelo, mas precisa depois fazer os teste de resíduo
completop = lm(WINP ~ ., data = dados_regressaop)
vaziop = lm(WINP ~ 1, data = dados_regressaop)
step(completop, scope=list(upper=completop, lower=vaziop), direction='backward', trace=TRUE)

# lm(formula = WINP ~ TEAM + PTS + FGM + FGA + FGP + `3PM` + FTM + 
#OREB + DREB + REB + PF + PlusMinus, data = dados_regressaop) 

modelo_backp <- lm(formula = WINP ~ TEAM + PTS + FGM + FGA + FGP + `3PM` + FTM + OREB + DREB + REB + PF + PlusMinus, data = dados_regressaop) 
modelo_backp
coef(modelo_backp)
anova(modelo_backp)
summary(modelo_backp) #Adjusted R-squared: 0.783 

#### Forward Selection ########
completop = lm(WINP ~ ., data = dados_regressaop)
vaziop = lm(WINP ~ 1, data = dados_regressaop)
step(vaziop, scope=list(upper=completop, lower=vaziop), direction='forward', trace=TRUE)

#lm(formula = WINP ~ PlusMinus + TEAM + DREB + BLKA, data = dados_regressaop)

modelo_forwp <- lm(formula = WINP ~ PlusMinus + TEAM + DREB + BLKA, data = dados_regressaop)
modelo_forwp
coef(modelo_forwp)
anova(modelo_forwp)
summary(modelo_forwp) #Adjusted R-squared:  0.7406 

############## Anova ###############
modelop1 #Completo
modelop4 #PTS + FGM + FGP + `3PM` + FTM + PF + PlusMinus
modelop5 #PTS + FGM + `3PM` + FTM + PF + PlusMinus
modelo_backp # TEAM + PTS + FGM + FGA + FGP + `3PM` + FTM + OREB + DREB + REB + PF + PlusMinus
modelo_forwp #PlusMinus + TEAM + DREB + BLKA
modelo_vazio <-lm(WINP ~ 1, data = dados_regressaop)
modelo_plus <-lm(WINP ~ PlusMinus, data = dados_regressaop)
modelo_dreb <- lm(WINP ~ DREB + PlusMinus, data = dados_regressaop)
modelo_blka <- lm(WINP ~ BLKA + DREB + PlusMinus, data = dados_regressaop)
modelo_pf <- lm(WINP ~ PF + DREB + PlusMinus, data = dados_regressaop)
modelo_reb <- lm(WINP ~ REB + DREB + PlusMinus, data = dados_regressaop)
modelo_oreb <- lm(WINP ~ OREB + DREB + PlusMinus, data = dados_regressaop)
modelo_ftm <- lm(WINP ~ FTM + DREB + PlusMinus, data = dados_regressaop)
modelo_pfd <- lm(WINP ~ PFD + DREB + PlusMinus, data = dados_regressaop)
modelo_blk <- lm(WINP ~ BLK + DREB + PlusMinus, data = dados_regressaop)
modelo_stl <- lm(WINP ~ STL + DREB + PlusMinus, data = dados_regressaop)
modelo_tov <- lm(WINP ~ TOV + DREB + PlusMinus, data = dados_regressaop)
modelo_ast <- lm(WINP ~ AST + DREB + PlusMinus, data = dados_regressaop)
modelo_fta <- lm(WINP ~ FTA + DREB + PlusMinus, data = dados_regressaop)
modelo_ftp <- lm(WINP ~ FTP + DREB + PlusMinus, data = dados_regressaop)
modelo_fga <- lm(WINP ~ FGA + DREB + PlusMinus, data = dados_regressaop)
modelo_fgp <- lm(WINP ~ FGP + DREB + PlusMinus, data = dados_regressaop)
modelo_fgm <- lm(WINP ~ FGM + DREB + PlusMinus, data = dados_regressaop)
modelo_pts <- lm(WINP ~ PTS + DREB + PlusMinus, data = dados_regressaop)
modelo_team <- lm(WINP ~ TEAM + DREB + PlusMinus, data = dados_regressaop)

anova(modelo_vazio, modelo_plus) #PlusMinus significativo
anova(modelo_plus, modelo_dreb) #0.09805, DREB significativo
anova(modelo_dreb, modelo_blka) #BLKA não significativo
anova(modelo_dreb, modelo_pf) #PF não significativo
anova(modelo_dreb, modelo_reb) #REB não significativo
anova(modelo_dreb, modelo_oreb) #OREB não significativo
anova(modelo_dreb, modelo_ftm) #FTM não significativo
anova(modelo_dreb, modelo_pfd) #PFD não significativo
anova(modelo_dreb, modelo_blk) #BLK não significativo
anova(modelo_dreb, modelo_stl) #STL não significativo
anova(modelo_dreb, modelo_tov) #TOV não significativo
anova(modelo_dreb, modelo_ast) #AST não significativo
anova(modelo_dreb, modelo_fta) #FTA não significativo
anova(modelo_dreb, modelo_ftp) #FTP não significativo
anova(modelo_dreb, modelo_fgp) #FGP não significativo
anova(modelo_dreb, modelo_fga) #FGPAnão significativo
anova(modelo_dreb, modelo_fgm) #FGM não significativo
anova(modelo_dreb, modelo_pts) #FGM não significativo
anova(modelo_dreb, modelo_team) #TEAM significativo

#melhor modelo é o modelo_team com PlusMinus, DREB e TEAM

############## Análise de resíduos ############

####### Modelo completo ########
plot(modelop1, which = 1)
plot(modelop1, which = 2)
plot(modelop1, which = 3)
plot(modelop1, which = 4)
plot(modelop1, which = 5)
plot(modelop1, which = 6)
shapiro.test(modelop1$residuals) #p-value = 0.001294, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelop1) #p-value = 0.1243
#Independência
plot(modelop1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelop1$fitted.values, modelop1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelop1) #p-value = 0.004251, heterocedasticidade


###### Modelo 2 #######
plot(modelop2, which = 1)
plot(modelop2, which = 2)
plot(modelop2, which = 3)
plot(modelop2, which = 4)
plot(modelop2, which = 5)
plot(modelop2, which = 6)
shapiro.test(modelop2$residuals) #p-value = 1.682e-05, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelop2) #p-value = 
#Independência
plot(modelop2$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelop2$fitted.values, modelop3$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelop2) #p-value = 

###### Modelo 3 #######
plot(modelop3, which = 1)
plot(modelop3, which = 2)
plot(modelop3, which = 3)
plot(modelop3, which = 4)
plot(modelop3, which = 5)
plot(modelop3, which = 6)
shapiro.test(modelop3$residuals) #p-value = 1.682e-05, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelop3) #p-value = 0.07474
#Independência
plot(modelop3$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelop3$fitted.values, modelop3$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelop3) #p-value = 0.001571, heterocedasticidade

###### Modelo 4 #######
plot(modelop4, which = 1)
plot(modelop4, which = 2)
plot(modelop4, which = 3)
plot(modelop4, which = 4)
plot(modelop4, which = 5)
plot(modelop4, which = 6)
shapiro.test(modelop4$residuals) #p-value = 1.682e-05, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelop4) #p-value = 0.07474
#Independência
plot(modelop4$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelop4$fitted.values, modelop4$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelop4) #p-value = 0.001571, heterocedasticidade

###### Modelo 5 #######
plot(modelop5, which = 1)
plot(modelop5, which = 2)
plot(modelop5, which = 3)
plot(modelop5, which = 4)
plot(modelop5, which = 5)
plot(modelop5, which = 6)
shapiro.test(modelop5$residuals) #p-value = 1.682e-05, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelop5) #p-value = 0.07474
#Independência
plot(modelop5$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelop5$fitted.values, modelop5$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelop5) #p-value = 
#### Backward ##########
plot(modelo_backp, which = 1)
plot(modelo_backp, which = 2)
plot(modelo_backp, which = 3)
plot(modelo_backp, which = 4)
plot(modelo_backp, which = 5)
plot(modelo_backp, which = 6)
shapiro.test(modelo_backp$residuals) #p-value = 0.002993,não normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_backp) #p-value = 0.07689
#Independência
plot(modelo_backp$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_backp$fitted.values, modelo_backp$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_backp) #p-value = 0.002445, heterocedasticidade

#### Backward 5% ##########
plot(modelo_backp1, which = 1)
plot(modelo_backp1, which = 2)
plot(modelo_backp1, which = 3)
plot(modelo_backp1, which = 4)
plot(modelo_backp1, which = 5)
plot(modelo_backp1, which = 6)
shapiro.test(modelo_backp1$residuals) #p-value = 0.002993,não normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_backp1) #p-value = 0.07689
#Independência
plot(modelo_backp1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_backp1$fitted.values, modelo_backp1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_backp1) #p-value = 0.002445, heterocedasticidade
#### Forward ######
plot(modelo_forwp, which = 1)
plot(modelo_forwp, which = 2)
plot(modelo_forwp, which = 3)
plot(modelo_forwp, which = 4)
plot(modelo_forwp, which = 5)
plot(modelo_forwp, which = 6)
shapiro.test(modelo_forwp$residuals) #p-value = 0.1997, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_forwp) #p-value = 0.07378
#Independência
plot(modelo_forwp$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_forwp$fitted.values, modelo_forwp$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_forwp) #p-value = 1.981e-05, heterocedasticidade
