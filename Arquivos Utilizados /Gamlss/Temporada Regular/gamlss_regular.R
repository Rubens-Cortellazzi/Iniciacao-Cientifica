#Quanto menor o AIC melhor.

source("dados_regular.R")

#Pacote que será utilizado para o gamlss
#install.packages("gamlss")
library(gamlss)

### Aplicação da modelagem 

######## Beta ###############
gamlss.family(BE)
#### Modelo Completo família beta ####
modelo_gamlss <- gamlss(WINP ~ ., data = dados_regressao, family = BE)
modelo_gamlss
coef(modelo_gamlss)
summary(modelo_gamlss) #AIC:     -1617.542 

### Modelo com variáveis significantes em 10% ####
modelo_gamlss1 <- gamlss(WINP ~ PTS + FGM + FTM + FTA + TOV + STL + PlusMinus, data = dados_regressao, family = BE)
modelo_gamlss1
coef(modelo_gamlss1)
summary(modelo_gamlss1) #Só Plus Minus foi significativo.
#AIC:     -1646 

############# Forward Selection beta ###########
gamlss_completo = gamlss(WINP ~ ., data = dados_regressao, family = BE)
gamlss_vazio = gamlss(WINP ~ 1, data = dados_regressao, family = BE)
step(gamlss_vazio, scope=list(upper=gamlss_completo, lower=gamlss_vazio), direction='forward', trace=TRUE)

# Call:  gamlss(formula = WINP ~ PlusMinus + FGP + PTS + PF,      family = BE, data = dados_regressao) 
# 
# Mu Coefficients:
#   (Intercept)    PlusMinus          FGP          PTS           PF  
# -0.505065     0.131669     0.023267    -0.003065    -0.012155  
# Sigma Coefficients:
#   (Intercept)  
# -2.458  
# 
# Degrees of Freedom for the fit: 6 Residual Deg. of Freedom   444 
# Global Deviance:     -1676.84 
# AIC:     -1664.84 
# SBC:     -1640.19 

gamlss_beta_forw = gamlss(formula = WINP ~ PlusMinus + FGP + PTS + PF, family = BE, data = dados_regressao)
gamlss_beta_forw
coef(gamlss_beta_forw)
summary(gamlss_beta_forw) #AIC:     -1664.843

###### backward regression beta ########
#Selecão das variáveis para compor o modelo, mas precisa depois fazer os teste de resíduo
gamlss_completo = gamlss(WINP ~ ., data = dados_regressao, family = BE)
gamlss_vazio = gamlss(WINP ~ 1, data = dados_regressao, family = BE)
step(gamlss_completo, scope=list(upper=gamlss_completo, lower=gamlss_vazio), direction='backward', trace=TRUE)

# Call:  gamlss(formula = WINP ~ PTS + FGA + FGP + `3PA` + `3PP` + FTM + PF + PlusMinus, family = BE, data = dados_regressao) 
# 
# Mu Coefficients:
#(Intercept)      PTS          FGA          FGP        `3PA`        `3PP`          FTM           PF      PlusMinus  
# -7.32808     -0.08135      0.07156      0.15412      0.02814      0.02426      0.07866     -0.01374      0.13087  
# Sigma Coefficients:
#   (Intercept)  
# -2.466  
# 
# Degrees of Freedom for the fit: 10 Residual Deg. of Freedom   440 
# Global Deviance:     -1682.93 
# AIC:     -1662.93 
# SBC:     -1621.84 

gamlss_beta_back <- gamlss(formula = WINP ~ PTS + FGA + FGP + `3PA` + `3PP` + FTM + PF + PlusMinus, family = BE, data = dados_regressao) 
gamlss_beta_back
coef(gamlss_beta_back)
summary(gamlss_beta_back) #AIC:     -1662.933

################# Normal ################
gamlss.family(NO)
#### Modelo Completo família normal ####
modelo_gamlssN <- gamlss(WINP ~ ., data = dados_regressao, family = NO)
modelo_gamlssN
coef(modelo_gamlssN)
summary(modelo_gamlssN)
#AIC:     -1624.372 

###Modelo normal com variáveis significativas 10% ####
modelo_gamlssN1 <- gamlss(WINP ~ TOV + STL + OREB + DREB + FTA + FTM + PlusMinus, data = dados_regressao, family = NO)
modelo_gamlssN1
coef(modelo_gamlssN1)
summary(modelo_gamlssN1) #OREB e Plus Minus significativo
#AIC:     -1638.776 

########## Forward Selection Normal ######################
gamlss_completoN = gamlss(WINP ~ ., data = dados_regressao, family = NO)
gamlss_vazioN = gamlss(WINP ~ 1, data = dados_regressao, family = NO)
step(gamlss_vazioN, scope=list(upper=gamlss_completoN, lower=gamlss_vazioN), direction='forward', trace=TRUE)

# Call:  gamlss(formula = WINP ~ PlusMinus + PF + FGP + FGM,      family = NO, data = dados_regressao) 
# 
# Mu Coefficients:
#   (Intercept)    PlusMinus           PF          FGP          FGM  
# 0.401565     0.030261    -0.003478     0.005746    -0.002433  
# Sigma Coefficients:
#   (Intercept)  
# -3.262  
# 
# Degrees of Freedom for the fit: 6 Residual Deg. of Freedom   444 
# Global Deviance:     -1658.44 
# AIC:     -1646.44 
# SBC:     -1621.78 

gamlss_normal_forw <- gamlss(formula = WINP ~ PlusMinus + PF + FGP + FGM, family = NO, data = dados_regressao) 
gamlss_normal_forw
coef(gamlss_normal_forw)
summary(gamlss_normal_forw) #-1646.44

###### backward regression Normal #####
gamlss_completoN = gamlss(WINP ~ ., data = dados_regressao, family = NO)
gamlss_vazioN = gamlss(WINP ~ 1, data = dados_regressao, family = NO)
step(gamlss_completoN, scope=list(upper=gamlss_completoN, lower=gamlss_vazioN), direction='backward', trace=TRUE)

# gamlss(formula = WINP ~ TEAM + PTS + FGP + `3PM` +  
#FTM + FTA + FTP + OREB + DREB + TOV + STL + PFD +  
#  PlusMinus, family = NO, data = dados_regressao)
gamlss_normal_back <- gamlss(formula = WINP ~ TEAM + PTS + FGP + `3PM` +  
                               FTM + FTA + FTP + OREB + DREB + TOV + STL + PFD +  
                               PlusMinus, family = NO, data = dados_regressao)
gamlss_normal_back
coef(gamlss_normal_back)
summary(gamlss_normal_back) #AIC:     -1645.353 

############### Análise ANOVA #############
##### Beta ######
modelo_gamlss1 #PTS + FGM + FTM + FTA + TOV + STL + PlusMinus 
gamlss_beta_back # PTS + FGA + FGP + `3PA` + `3PP` + FTM + PF + PlusMinus
gamlss_beta_forw #PlusMinus + FGP + PTS + PF
modelo_beta0 <- gamlss(WINP ~ 1, data = dados_regressao, family = BE)
modelo_beta_plus <- gamlss(WINP ~ PlusMinus, data = dados_regressao, family = BE)
modelo_beta_fgp <- gamlss(WINP ~ PlusMinus + FGP, data = dados_regressao, family = BE)
modelo_beta_pts <- gamlss(WINP ~ PlusMinus + FGP + PTS, data = dados_regressao, family = BE)
modelo_beta_pf <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF, data = dados_regressao, family = BE)
modelo_beta_pf_ftm <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + FTM, data = dados_regressao, family = BE)
modelo_beta_pf_3pp <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + `3PP`, data = dados_regressao, family = BE)
modelo_beta_pf_3pa <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + `3PA`, data = dados_regressao, family = BE)
modelo_beta_pf_fga <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + FGA, data = dados_regressao, family = BE)
modelo_beta_pf_fgm <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + FGM, data = dados_regressao, family = BE)
modelo_beta_pf_3pm <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + `3PM`, data = dados_regressao, family = BE)
modelo_beta_pf_fta <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + FTA, data = dados_regressao, family = BE)
modelo_beta_pf_ftp <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + FTP, data = dados_regressao, family = BE)
modelo_beta_pf_reb <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + REB, data = dados_regressao, family = BE)
modelo_beta_pf_oreb <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + OREB, data = dados_regressao, family = BE)
modelo_beta_pf_dreb <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + DREB, data = dados_regressao, family = BE)
modelo_beta_pf_ast <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + AST, data = dados_regressao, family = BE)
modelo_beta_pf_tov <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + TOV, data = dados_regressao, family = BE)
modelo_beta_pf_stl <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + STL, data = dados_regressao, family = BE)
modelo_beta_pf_blk <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + BLK, data = dados_regressao, family = BE)
modelo_beta_pf_blka <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + BLKA, data = dados_regressao, family = BE)
modelo_beta_pf_pfd <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + PFD, data = dados_regressao, family = BE)
modelo_beta_pf_team <- gamlss(WINP ~ PlusMinus + FGP + PTS + PF + TEAM, data = dados_regressao, family = BE)

lrtest(modelo_beta0, modelo_beta_plus) #2.2e-16 plusMinus deu significativo
lrtest(modelo_beta_plus, modelo_beta_fgp)#0.01844, FGP significativo
lrtest(modelo_beta_fgp, modelo_beta_pts)#0.02115, PTS significativo
lrtest(modelo_beta_pts, modelo_beta_pf)#0.03149, PF significativo
lrtest(modelo_beta_pf, modelo_beta_pf_ftm)#0.9896, FTM não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_3pp)#0.274, 3pp não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_3pa)#0.6111, 3pa não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_fga)#0.716, FGA não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_fgm)# FGM não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_3pm)# 3PM não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_fta)# fta não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_ftp)# ftp não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_reb)# reb não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_oreb)# oreb não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_dreb)#dreb não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_ast)#ast não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_tov)#tov não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_stl)#stl não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_blk)#blk não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_blka)#blka não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_pfd)#pfd não significativo
lrtest(modelo_beta_pf, modelo_beta_pf_team)#TEAM significante significativo

#modelo_beta_pf_team foi o melhor modelo analisado com PlusMinus + FGP + PTS + PF + TEAM

##### Normal ######
modelo_gamlssN1 #TOV + STL + PF + PlusMinus
gamlss_normal_back #TEAM + PTS + FGP + `3PM` +FTM + FTA + FTP + OREB + DREB + TOV + STL + PFD +  PlusMinus
gamlss_normal_forw #PlusMinus + PF + FGP + FGM
modelo_normal0 <- gamlss(formula = WINP ~1, family = NO, data = dados_regressao) 
modelo_normal_plusMinus <- gamlss(formula = WINP ~ PlusMinus, family = NO, data = dados_regressao) 
modelo_normal_pf <- gamlss(formula = WINP ~ PlusMinus + PF, family = NO, data = dados_regressao) 
modelo_normal_fgp <- gamlss(formula = WINP ~ PlusMinus + PF + FGP, family = NO, data = dados_regressao) 
modelo_normal_stl <- gamlss(formula = WINP ~ PlusMinus + PF + FGP + STL, family = NO, data = dados_regressao) 
modelo_normal_tov <- gamlss(formula = WINP ~ PlusMinus + PF + FGP + TOV, family = NO, data = dados_regressao) 
modelo_normal_fgm <- gamlss(formula = WINP ~ PlusMinus + PF + FGP + FGM, family = NO, data = dados_regressao) 
modelo_normal_pts <- gamlss(formula = WINP ~ PlusMinus + PF + FGP + FGM + PTS, family = NO, data = dados_regressao) 
modelo_normal_team <- gamlss(formula = WINP ~ PlusMinus + PF + FGP + FGM + TEAM, family = NO, data = dados_regressao) 

lrtest(modelo_normal0, modelo_normal_plusMinus) #2.2e-16, plusminus deu significativo
lrtest(modelo_normal_plusMinus, modelo_normal_pf) #0.01189, PF deu significativo
lrtest(modelo_normal_pf, modelo_normal_fgp) #0.03331, FGP deu significativo
lrtest(modelo_normal_fgp, modelo_normal_stl) #0.8083, STL deu não significativo
lrtest(modelo_normal_fgp, modelo_normal_tov) #0.8184, TOV deu não significativo
lrtest(modelo_normal_fgp, modelo_normal_fgm) #0.02268, FGM deu significativo
lrtest(modelo_normal_fgm, modelo_normal_pts) #PTS deu não significativo
lrtest(modelo_normal_fgm, modelo_normal_team) # TEAM deu significativo

#Melhor modelo é modelo_normal_team com PlusMinus + PF + FGP + FGM + TEAM
#Mesmo que a regressão linear

############## Análise de Resíduos ##############
##### Beta #####
###### Modelo Completo #######
plot(modelo_gamlss)
shapiro.test(modelo_gamlss$residuals) #p-value = 0.2651, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_gamlss) #p-value = 0.2282
#Independência
plot(modelo_gamlss$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(modelo_gamlss) #p-value = 0.04578

###### Modelo 10% #######
plot(modelo_gamlss1)
shapiro.test(modelo_gamlss1$residuals) #p-value = 0.3737, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_gamlss1) #p-value = 0.2548
#Independência
plot(modelo_gamlss1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(modelo_gamlss1) #p-value = 0.09055 , homocedastico

##### Forward ####
plot(gamlss_beta_forw)
shapiro.test(gamlss_beta_forw$residuals) #p-value = 0.2853, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_beta_forw) #p-value = 0.1735
#Independência
plot(gamlss_beta_forw$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_beta_forw) #p-value = 0.0006407

##### Bacward ####
plot(gamlss_beta_back)
shapiro.test(gamlss_beta_back$residuals) #p-value = 0.2253, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_beta_back) #p-value = 0.2641
#Independência
plot(gamlss_beta_back$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_beta_back) #p-value = 0.01486


##### Normal #####
###### Modelo Completo #######
plot(modelo_gamlssN)
shapiro.test(modelo_gamlssN$residuals) #p-value = 0.1997, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_gamlssN) #p-value = 0.2282
#Independência
plot(modelo_gamlssN$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(modelo_gamlssN) #p-value = 0.04578

###### Modelo Normal 10% #######
plot(modelo_gamlssN1)
shapiro.test(modelo_gamlssN1$residuals) #p-value = 0.1847, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_gamlssN1) #p-value = 0.2497
#Independência
plot(modelo_gamlssN1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(modelo_gamlssN1) #p-value = 0.001367

##### Forward Normal  ####
plot(gamlss_normal_forw)
shapiro.test(gamlss_normal_forw$residuals) #p-value = 0.2296, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_normal_forw) #p-value =  0.195
#Independência
plot(gamlss_normal_forw$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_normal_forw) #p-value = 0.001575

##### Backward Normal ####
plot(gamlss_normal_back)
shapiro.test(gamlss_normal_back$residuals) #p-value = 0.2669, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(gamlss_normal_back) #p-value = 0.1735
#Independência
plot(gamlss_normal_back$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Breusch_Pagan para homocedasticdade
bptest(gamlss_normal_back) #p-value = 0.0006407