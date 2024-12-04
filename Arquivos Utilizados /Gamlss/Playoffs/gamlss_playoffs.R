######## Gamlss Playoffs #############
source("dados_playoffs.R")

#install.packages("gamlss")
library(gamlss)

########## Aplicando regressão ##########

#### Família Beta ####
#modelo_gamlssp <- gamlss(WINP ~ ., data = dados_regressaop, family = BE()) não funciona
#NÃO VOU UTILIZAR

#### Família BEINF ####
#Não vou utilizar também porque no meu banco de dados não tem observações 1 
# BEINF é para a variável resposta segue a distribuição beta em que o 0 e 1 está incluso;
gamlss.family(BEINF)
modelo_gamlssp1 <- gamlss(WINP ~ ., data = dados_regressaop, family = BEINF()) #BEINF() é a beta inflacionada, em que assume 0 e 1 na beta.
summary(modelo_gamlssp1)

#### Família BEZI ####
#Vou utilizar 
# BEZI é para a variável resposta segue a distribuição beta em que o 0 está incluso;
gamlss.family(BEZI)
BEZI()
####Modelo Completo ####
modelo_gamlssp_bezi1 <- gamlss(WINP ~ ., data = dados_regressaop, family = BEZI()) #BEZI() é a beta zero inflascionada, em que assume 0 na beta.
modelo_gamlssp_bezi1
coef(modelo_gamlssp_bezi1)
car::Anova(modelo_gamlssp_bezi1)

#### Modelo 10% ####
modelo_gamlssp_bezi11 <- gamlss(WINP ~ PTS + FGM + FGA + FGP + `3PM` + FTM + STL + PF + PlusMinus, data = dados_regressaop, family = BEZI()) #BEINF() é a beta zero inflascionada, em que assume 0 na beta.
modelo_gamlssp_bezi11
coef(modelo_gamlssp_bezi11)
car::Anova(modelo_gamlssp_bezi11)
AIC(modelo_gamlssp_bezi11)

#### Forward bezi #####
gamlssp_completo = gamlss(WINP ~ ., data = dados_regressaop, family = BEZI())
gamlssp_vazio = gamlss(WINP ~ 1, data = dados_regressaop, family = BEZI())
step(gamlssp_vazio, scope=list(upper=gamlssp_completo, lower=gamlssp_vazio), direction='forward', trace=TRUE)

# Call:  gamlss(formula = WIN_P ~ Plus_Minus + PF + BLKA, family = BEZI(),  
#               data = dados_regressaop) 
# 
# Mu Coefficients:
#   (Intercept)   Plus_Minus           PF         BLKA  
# 0.63265      0.10445     -0.02562     -0.02786  
# Sigma Coefficients:
#   (Intercept)  
# 3.581  
# Nu Coefficients:
#   (Intercept)  
# -2.197  
# 
# Degrees of Freedom for the fit: 6 Residual Deg. of Freedom   234 
# Global Deviance:     -332.923 
# AIC:     -320.923 
# SBC:     -300.039 

gamlss_betap_forw = gamlss(formula = WINP ~ PlusMinus + PF + BLKA, family = BEZI(), data = dados_regressaop) 
gamlss_betap_forw
coef(gamlss_betap_forw)
summary(gamlss_betap_forw) #BLKA não foi significante. 
AIC(gamlss_betap_forw)#AIC:     -320.9232

###### backward bezi ########
#Selecão das variáveis para compor o modelo, mas precisa depois fazer os teste de resíduo
gamlssp_completo = gamlss(WINP ~ ., data = dados_regressaop, family = BEZI())
gamlssp_vazio = gamlss(WINP ~ 1, data = dados_regressaop, family = BEZI())
step(gamlssp_completo, scope=list(upper=gamlssp_completo, lower=gamlssp_vazio), direction='backward', trace=TRUE)

# call:  gamlss(formula = WIN_P ~ FGM + FGA + FGP + FTP +  
#                PF + Plus_Minus, family = BEZI(), data = dados_regressaop) 
# 
# Mu Coefficients:
#   (Intercept)          FGM          FGA         FGP         FTP  
# -11.554659    -0.308306     0.133228     0.266678     0.007406  
# PF   Plus_Minus  
# -0.028560     0.103676  
# Sigma Coefficients:
#   (Intercept)  
# 3.602  
# Nu Coefficients:
#   (Intercept)  
# -2.197  
# 
# Degrees of Freedom for the fit: 9 Residual Deg. of Freedom   231 
# Global Deviance:     -337.15 
# AIC:     -319.15 
# SBC:     -287.824 

gamlss_betap_back <- gamlss(formula = WINP ~ FGM + FGA + FGP + FTP + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_back
coef(gamlss_betap_back)
summary(gamlss_betap_back) #FT_P não significativo
AIC(gamlss_betap_back) #AIC:     -319.1499

###### Anova ##########
modelo_gamlssp_bezi1
modelo_gamlssp_bezi11 #PTS + FGM + FGA + FGP + `3PM` + FTM + STL + PF + PlusMinus
gamlss_betap_back #FGM + FGA + FGP + FTP + PF + PlusMinus
gamlss_betap_forw #PlusMinus + PF + BLKA
gamlss_betap_plus <- gamlss(formula = WINP ~ PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_pf <- gamlss(formula = WINP ~ PF + PlusMinus, family = BEZI(),data = dados_regressaop)
gamlss_betap_pfd <- gamlss(formula = WINP ~ PFD + PF + PlusMinus, family = BEZI(),data = dados_regressaop)
gamlss_betap_blka <- gamlss(formula = WINP ~ BLKA + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_blk <- gamlss(formula = WINP ~ BLK + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_stl <- gamlss(formula = WINP ~ STL + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_tov <- gamlss(formula = WINP ~ TOV + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_ast <- gamlss(formula = WINP ~ AST + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_reb <- gamlss(formula = WINP ~ REB + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_oreb <- gamlss(formula = WINP ~ OREB + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_dreb <- gamlss(formula = WINP ~ DREB + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_fgm <- gamlss(formula = WINP ~ FGM + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_fga <- gamlss(formula = WINP ~ FGA + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_fgp <- gamlss(formula = WINP ~ FGP + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_ftp <- gamlss(formula = WINP ~ FTP + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_pts <- gamlss(formula = WINP ~ PTS + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_3pm <- gamlss(formula = WINP ~ `3PM` + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_ftm <- gamlss(formula = WINP ~ FTM + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_fta <- gamlss(formula = WINP ~ FTA + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_3pa <- gamlss(formula = WINP ~ `3PA` + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_3pp <- gamlss(formula = WINP ~ `3PP` + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_3pp <- gamlss(formula = WINP ~ `3PP` + PF + PlusMinus, family = BEZI(), data = dados_regressaop)
gamlss_betap_team <- gamlss(formula = WINP ~ TEAM + PF + PlusMinus, family = BEZI(), data = dados_regressaop)

lrtest(gamlss_betap_plus, gamlss_betap_pf) #0.01893, PF deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_pfd) #PFD não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_blka) #0.1333, BLKA não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_blk) #BLK não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_stl) #stl não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_tov) #tov não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_ast) #ast não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_reb) #reb não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_oreb) #oreb não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_dreb) #dreb não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_fgm) #0.9587, FGM não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_fga) #0.4589, FGA não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_fgp) #0.3049, FGP não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_ftp) #0.1863, FTP não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_pts) #0.8541, PTS não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_3pm) #0.8687, 3PM não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_ftm) #FTM não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_fta) #FTA não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_3pa) #3PA não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_3pp) #3PP não deu significativo
lrtest(gamlss_betap_pf, gamlss_betap_team) #TEAM deu significativo
#Melhor modelo é gamlss_betap_pf com PF + PlusMinus + TEAM. 

#### Família Normal ####
gamlss.family(NO)
#Mesmo odelo que a regressão linear
### Modelo Completo####
modelo_gamlssp_N1 <- gamlss(WINP ~ ., data = dados_regressaop, family = NO()) 
modelo_gamlssp_N1
coef(modelo_gamlssp_N1)
summary(modelo_gamlssp_N1) 
AIC(modelo_gamlssp_N1)#AIC:     -384.564 

### Modelo 5% ####
modelo_gamlssp_N2 <- gamlss(WINP ~ PTS + FGM + `3PM` + FTM + PF + PlusMinus, data = dados_regressaop, family = NO()) 
modelo_gamlssp_N2
coef(modelo_gamlssp_N2)
summary(modelo_gamlssp_N2) #Todas as variáveis significante
AIC(modelo_gamlssp_N2) #AIC:     -399.976

### Modelo 10% ####
modelo_gamlssp_N3 <- gamlss(WINP ~ PTS + FGM + `3PM` + FTM + OREB + DREB + REB + PF + PlusMinus, data = dados_regressaop, family = NO()) 
modelo_gamlssp_N3
coef(modelo_gamlssp_N3)
summary(modelo_gamlssp_N3) #PF não foi significativo
AIC(modelo_gamlssp_N3)#AIC:     -400.0948

#### Forward normal #####
gamlssp_completoN = gamlss(WINP ~ ., data = dados_regressaop, family = NO())
gamlssp_vazioN = gamlss(WINP ~ 1, data = dados_regressaop, family = NO())
step(gamlssp_vazioN, scope=list(upper=gamlssp_completoN, lower=gamlssp_vazioN), direction='forward', trace=TRUE)

# Call:  gamlss(formula = WIN_P ~ Plus_Minus + DREB, family = NO(),  
#               data = dados_regressaop) 
# 
# Mu Coefficients:
#   (Intercept)   Plus_Minus         DREB  
# 0.345213     0.025682     0.004033  
# Sigma Coefficients:
#   (Intercept)  
# -2.268  
# 
# Degrees of Freedom for the fit: 4 Residual Deg. of Freedom   236 
# Global Deviance:     -407.576 
# AIC:     -399.576 
# SBC:     -385.653 

gamlss_betap_forwN = gamlss(formula = WINP ~ PlusMinus + DREB, family = NO(), data = dados_regressaop)
gamlss_betap_forwN
coef(gamlss_betap_forwN)
summary(gamlss_betap_forwN) #AIC:     -399.576 

###### backward normal ########
#Cheogou no mesmo modelo que lm
#Selecão das variáveis para compor o modelo, mas precisa depois fazer os teste de resíduo
gamlssp_completoN = gamlss(WIN_P ~ ., data = dados_regressaop, family = NO())
gamlssp_vazioN = gamlss(WIN_P ~ 1, data = dados_regressaop, family = NO())
step(gamlssp_completoN, scope=list(upper=gamlssp_completoN, lower=gamlssp_vazioN), direction='backward', trace=TRUE)

# Call:  gamlss(formula = WIN_P ~ PTS + FGM + `3PM` + FTM +  
#                 FT_P + OREB + DREB + REB + PF + PFD + Plus_Minus,  
#               family = NO(), data = dados_regressaop) 
# 
# Mu Coefficients:
#   (Intercept)          PTS          FGM        `3PM`          FTM  
# 0.196477     0.269215    -0.539644    -0.270392    -0.272894  
# FT_P         OREB         DREB          REB           PF  
# 0.002872    -0.256336    -0.252691     0.256559    -0.006153  
# PFD   Plus_Minus  
# 0.008881     0.025024  
# Sigma Coefficients:
#   (Intercept)  
# -2.305  
# 
# Degrees of Freedom for the fit: 13 Residual Deg. of Freedom   227 
# Global Deviance:     -425.078 
# AIC:     -399.078 
# SBC:     -353.83 

gamlss_betap_backN <- gamlss(formula = WIN_P ~ PTS + FGM + `3PM` + FTM + FT_P + OREB + DREB + REB + PF + PFD + Plus_Minus,family = NO(), data = dados_regressaop) 
gamlss_betap_backN
coef(gamlss_betap_backN)
summary(gamlss_betap_backN) #FTP e PFD não deu significante.
AIC(gamlss_betap_backN)#AIC:     -399.0784 

#### Backward 5% ####
gamlss_betap_backN1 <- gamlss(formula = WIN_P ~ PTS + FGM + `3PM` + FTM + Plus_Minus, family = NO(), data = dados_regressaop) 
gamlss_betap_backN1
coef(gamlss_betap_backN1)
summary(gamlss_betap_backN1) 
AIC(gamlss_betap_backN1)#AIC:     -399.976

#### Backward 10% ####
gamlss_betap_backN2 <- gamlss(formula = WIN_P ~ PTS + FGM + `3PM` + FTM + OREB + DREB + REB + PF + Plus_Minus,family = NO(), data = dados_regressaop) 
gamlss_betap_backN2
coef(gamlss_betap_backN2)
summary(gamlss_betap_backN2)#PF não foi significante (11%) e OREB, DREB e REB foram significantes a 10% 
AIC(gamlss_betap_backN2)#AIC:     -400.0948 

#### Análise de resíduos ####
### BEZI ####
## Modelo Completo ####
plot(modelo_gamlssp_bezi1)
shapiro.test(modelo_gamlssp_bezi1$residuals) #p-value = 0.0001865
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_gamlssp_bezi1) #p-value = 0.1243
#Independência
plot(modelo_gamlssp_bezi1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(modelo_gamlssp_bezi1) #p-value = 0.004251

## Modelo 10% ####
plot(modelo_gamlssp_bezi11)
shapiro.test(modelo_gamlssp_bezi11$residuals) #p-value = 9.489e-05
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_gamlssp_bezi11) #p-value = 0.09214
#Independência
plot(modelo_gamlssp_bezi11$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(modelo_gamlssp_bezi11) #p-value = 6.423e-05


### Forward BEZI####
plot(gamlss_betap_forw)
shapiro.test(gamlss_betap_forw$residuals) #p-value = 5.742e-05
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_betap_forw) #p-value = 0.1564
#Independência
plot(gamlss_betap_forw$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_betap_forw) #p-value = 

### Backward BEZI####
plot(gamlss_betap_back)
shapiro.test(gamlss_betap_back$residuals) #p-value =0.000135
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_betap_back) #p-value = 0.08308
#Independência
plot(gamlss_betap_back$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_betap_back) #p-value = 3.005e-05


### Normal ####
## Modelo Completo ####
plot(modelo_gamlssp_N1)
shapiro.test(modelo_gamlssp_N1$residuals) #p-value = 0.001294
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_gamlssp_N1) #p-value = 0.1243
#Independência
plot(modelo_gamlssp_N1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(modelo_gamlssp_N1) #p-value = 0.004251

## Modelo 5% ####
plot(modelo_gamlssp_N2)
shapiro.test(modelo_gamlssp_N2$residuals) #p-value = 0.0003381
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_gamlssp_N2) #p-value = 0.04164
#Independência
plot(modelo_gamlssp_N2$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(modelo_gamlssp_N2) #p-value = 0.001109

## Modelo 10% ####
plot(modelo_gamlssp_N3)
shapiro.test(modelo_gamlssp_N3$residuals) #p-value = 0.002322
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_gamlssp_N3) #p-value = 0.06851
#Independência
plot(modelo_gamlssp_N3$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(modelo_gamlssp_N3) #p-value = 0.0007679

### Forward ####
plot(gamlss_betap_forwN)
shapiro.test(gamlss_betap_forwN$residuals) #p-value = 5.082e-05
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_betap_forwN) #p-value = 0.07378
#Independência
plot(gamlss_betap_forwN$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_betap_forwN) #p-value = 1.981e-05

### Backward ####
plot(gamlss_betap_backN)
shapiro.test(gamlss_betap_backN$residuals) #p-value = 0.002993
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_betap_backN) #p-value = 0.07689
#Independência
plot(gamlss_betap_backN$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_betap_backN) #p-value = 0.002445

### Backward 5%####
plot(gamlss_betap_backN1)
shapiro.test(gamlss_betap_backN1$residuals) #p-value = 0.0003381
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_betap_backN1) #p-value = 0.04164
#Independência
plot(gamlss_betap_backN1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_betap_backN1) #p-value = 0.001109

### Backward 10%####
plot(gamlss_betap_backN2)
shapiro.test(gamlss_betap_backN2$residuals) #p-value = 0.002322
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_betap_backN2) #p-value = 0.06851
#Independência
plot(gamlss_betap_backN2$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_betap_backN2) #p-value = 0.0007679