source("dados_regular.R")

##### Regressão beta sem alterar a função de ligação. (logito) #######
## Modelo completo ##
modelo_beta1 <- betareg(WINP ~ .,data = dados_regressao) #Regressão com todos os dados do modelo
modelo_beta1
summary(modelo_beta1) #Pseudo R-squared: 0.9431
coef(modelo_beta1)
car::Anova(modelo_beta1)

### Modelo com variáveis significantes com alfa = 5% ##
modelo_beta11 <- betareg(WINP ~ FTM + TOV + STL + PlusMinus,data = dados_regressao) 
modelo_beta11
summary(modelo_beta11) #Pseudo R-squared: 0.930.
coef(modelo_beta11)
car::Anova(modelo_beta11) # Só plus minus significativo

### Modelo com variáveis significantes com alfa = 10% ###
modelo_beta12 <- betareg(WINP ~ FTM + FTA + TOV + STL + PlusMinus,data = dados_regressao) 
modelo_beta12
summary(modelo_beta12) #Pseudo R-squared: 0.9305 e Plus minus único significativo
coef(modelo_beta12)
car::Anova(modelo_beta12)

#####Fazendo a regressão beta, mas com loglog ######

### Com todas as variáveis do modelo ###
modelo_beta2 <- betareg(WINP ~ .,data = dados_regressao, link = "loglog") #Regressão com todos os dados do modelo
modelo_beta2
summary(modelo_beta2) #Pseudo R-squared: 0.9276
coef(modelo_beta2)

### com variáveis significantes com alfa = 5% ###
modelo_beta21 <- betareg(WINP ~ FTM + TOV + STL + PlusMinus,data = dados_regressao, link = "loglog") #Regressão com todos os dados do modelo
modelo_beta21
summary(modelo_beta21)#Só Plus Minus significativo 
#Pseudo R-squared: 0.9223
coef(modelo_beta21) 

### com variáveis significantes com alfa = 10%###
modelo_beta22 <- betareg(WINP ~ FTM + FTA + FTP + DREB + TOV + STL + PlusMinus,data = dados_regressao, link = "loglog") #Regressão com todos os dados do modelo
modelo_beta22
summary(modelo_beta22) #0.9225
#Só plusminus significativo
coef(modelo_beta22)

##### Fazendo a regressão beta, mas com probito #######

### Modelo completo ###
modelo_beta_probit <- betareg(WINP ~  .,data = dados_regressao, link = "probit")
modelo_beta_probit
summary(modelo_beta_probit) #Pseudo R-squared: 0.9303
coef(modelo_beta_probit)

###  Modelo com 5% ###
modelo_beta_probit1 <- betareg(WINP ~  FTM + TOV + STL + PlusMinus,data = dados_regressao, link = "probit")
modelo_beta_probit1
summary(modelo_beta_probit1) #Pseudo R-squared: 0.9319
#Só Plusminus significativo
coef(modelo_beta_probit1)

### Modelo com 10% ###
modelo_beta_probit2 <- betareg(WINP ~  FTM + FTA + TOV + STL + PlusMinus,data = dados_regressao, link = "probit")
modelo_beta_probit2
summary(modelo_beta_probit2) #Pseudo R-squared: 0.9319
#Só plusminus significativo
coef(modelo_beta_probit2)

##### Fazendo a regressão beta, mas com cloglog ####

### Modelo completo ###
modelo_beta_cloglog <- betareg(WINP ~  .,data = dados_regressao, link = "cloglog")
modelo_beta_cloglog
summary(modelo_beta_cloglog) #Pseudo R-squared: 0.937
coef(modelo_beta_cloglog)

### Modelo com 5% ###
modelo_beta_cloglog1 <- betareg(WINP ~  TOV + PlusMinus,data = dados_regressao, link = "cloglog")
modelo_beta_cloglog1
summary(modelo_beta_cloglog1) #Ambas significativas
#Pseudo R-squared: 0.9249
coef(modelo_beta_cloglog1)

### Modelo com 10% ###
modelo_beta_cloglog2 <- betareg(WINP ~ STL + TOV + PlusMinus,data = dados_regressao, link = "cloglog")
modelo_beta_cloglog2
summary(modelo_beta_cloglog2) #STL não significativo
#Pseudo R-squared: 0.9249
coef(modelo_beta_cloglog2)

##### Fazendo a regressão beta, mas com cauchito ### ######
### Modelo completo ###
modelo_beta_cauchit <- betareg(WINP ~  .,data = dados_regressao, link = "cauchit")
modelo_beta_cauchit
summary(modelo_beta_cauchit) #Pseudo R-squared: 0.9035
coef(modelo_beta_cauchit)

### Modelo com significância de 5% ###
modelo_beta_cauchit1 <- betareg(WINP ~  FTM + TOV + PlusMinus,data = dados_regressao, link = "cauchit")
modelo_beta_cauchit1
summary(modelo_beta_cauchit1) #Pseudo R-squared: 0.911
coef(modelo_beta_cauchit1)

############## Análise ANOVA ################
#### Logito#####
modelo_beta1 #Completo
modelo_beta11 #FTM + TOV + STL + PlusMinus
modelo_beta12 #FTM + FTA + TOV + STL + PlusMinus
modelo_beta1_vazio <- betareg(WINP ~ 1, data = dados_regressao)
modelo_beta1_plus <- betareg(WINP ~ PlusMinus, data = dados_regressao)
modelo_beta1_pf <- betareg(WINP ~ PF + PlusMinus, data = dados_regressao)
modelo_beta1_stl <- betareg(WINP ~ STL + PF + PlusMinus, data = dados_regressao)
modelo_beta1_3pp <- betareg(WINP ~ `3PP` + PF + PlusMinus, data = dados_regressao)
modelo_beta1_tov <- betareg(WINP ~ TOV + `3PP` + PF + PlusMinus, data = dados_regressao)
modelo_beta1_fta <- betareg(WINP ~ FTA + `3PP` + PF + PlusMinus, data = dados_regressao)
modelo_beta1_ftm <- betareg(WINP ~ FTM + `3PP` + PF + PlusMinus, data = dados_regressao)
modelo_beta1_fga <- betareg(WINP ~ FGA + `3PP` + PF + PlusMinus, data = dados_regressao)
modelo_beta1_fgp <- betareg(WINP ~ FGP + FGA + `3PP` + PF + PlusMinus, data = dados_regressao)
modelo_beta1_team <- betareg(WINP ~ TEAM + FGP + FGA + `3PP` + PF + PlusMinus, data = dados_regressao)

lrtest(modelo_beta1_vazio, modelo_beta1_plus) #PlusMinus significante
lrtest(modelo_beta1_plus, modelo_beta1_pf) #0.02665, PF melhorou o modelo
lrtest(modelo_beta1_pf, modelo_beta1_stl) #0.8447, STL não significante
lrtest(modelo_beta1_pf, modelo_beta1_3pp) #0.08035, 3PP significante
lrtest(modelo_beta1_3pp, modelo_beta1_tov) #0.8667, TOV não significante
lrtest(modelo_beta1_3pp, modelo_beta1_fta) #0.7767, FTA não significante
lrtest(modelo_beta1_3pp, modelo_beta1_ftm) #0.8735, FTM não significante
lrtest(modelo_beta1_3pp, modelo_beta1_fga) #0.08272, FGA significante
lrtest(modelo_beta1_fga, modelo_beta1_fgp) #0.02333, FGP significante
lrtest(modelo_beta1_fgp, modelo_beta1_team) #0.0243, team significante

#Melhor modelo logito é o modelo com TEAM + FGP + FGA + `3PP` + PF + PlusMinus que é modelo_beta1_team

##### loglog #######
modelo_beta2 #completo
modelo_beta21#STL + PF + PlusMinus
modelo_beta22#FTM + STL + PF + PlusMinus
modelo_beta2_vazio <- betareg(formula = WINP ~ 1, data = dados_regressao, link = "loglog")
modelo_beta2_plus <- betareg(formula = WINP ~ PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_pts <- betareg(formula = WINP ~ PTS + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_fga <- betareg(formula = WINP ~ FGA + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_fgm <- betareg(formula = WINP ~ FGM + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_fgp <- betareg(formula = WINP ~ FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_3pa <- betareg(formula = WINP ~ `3PA` + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_3pm <- betareg(formula = WINP ~ `3PM` + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_3pp <- betareg(formula = WINP ~ `3PP` + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_fta <- betareg(formula = WINP ~ FGP + FTA + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_ftm <- betareg(formula = WINP ~ FGP + FTM + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_ftp <- betareg(formula = WINP ~ FGP + FTP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_reb <- betareg(formula = WINP ~ REB + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_oreb <- betareg(formula = WINP ~ OREB + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_dreb <- betareg(formula = WINP ~ DREB + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_ast <- betareg(formula = WINP ~ AST + OREB + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_tov <- betareg(formula = WINP ~ TOV + OREB + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_stl <- betareg(formula = WINP ~ STL + OREB + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_blk <- betareg(formula = WINP ~  BLK + OREB + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_blka <- betareg(formula = WINP ~ BLKA + OREB + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_pf <- betareg(formula = WINP ~ PF + OREB + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_pfd <- betareg(formula = WINP ~ PFD + OREB + FGP + PlusMinus, data = dados_regressao, link = "loglog")
modelo_beta2_team <- betareg(formula = WINP ~ TEAM + PF + OREB + FGP + PlusMinus, data = dados_regressao, link = "loglog")

lrtest(modelo_beta2_vazio, modelo_beta2_plus) #2.2e-16, plusminus significativo
lrtest(modelo_beta2_plus, modelo_beta2_pts) #PTS não significativo
lrtest(modelo_beta2_plus, modelo_beta2_fga) #FGA não significativo
lrtest(modelo_beta2_plus, modelo_beta2_fgm) #0.8765, FGM não significativo
lrtest(modelo_beta2_plus, modelo_beta2_fgp) #FGP  significativo
lrtest(modelo_beta2_fgp, modelo_beta2_3pa) #3PA  não significativo
lrtest(modelo_beta2_fgp, modelo_beta2_3pm) #3PM  não significativo
lrtest(modelo_beta2_fgp, modelo_beta2_3pp) #3PP  não significativo
lrtest(modelo_beta2_fgp, modelo_beta2_fta) #FTA  não significativo
lrtest(modelo_beta2_fgp, modelo_beta2_ftm) #FTM  não significativo
lrtest(modelo_beta2_fgp, modelo_beta2_ftp) #FTP não significativo
lrtest(modelo_beta2_fgp, modelo_beta2_reb) #REB não significativo
lrtest(modelo_beta2_fgp, modelo_beta2_dreb) #DREB não significativo
lrtest(modelo_beta2_fgp, modelo_beta2_oreb) #OREB significativo
lrtest(modelo_beta2_oreb, modelo_beta2_ast) #AST não significativo
lrtest(modelo_beta2_oreb, modelo_beta2_tov) #TOV não significativo
lrtest(modelo_beta2_oreb, modelo_beta2_stl) #STL não significativo
lrtest(modelo_beta2_oreb, modelo_beta2_blk) #blk não significativo
lrtest(modelo_beta2_oreb, modelo_beta2_blka) #blka não significativo
lrtest(modelo_beta2_oreb, modelo_beta2_pfd) #pfd não significativo
lrtest(modelo_beta2_oreb, modelo_beta2_pf) #pf significativo
lrtest(modelo_beta2_pf, modelo_beta2_team) #team significativo

#Melhor modelo de loglog é o modelo_beta2_team com TEAM + PF + OREB + FGP + PlusMinus;

##### Probito #######
modelo_beta_probit_vazio <- betareg(formula = WINP ~ 1, data = dados_regressao, link = "probit")
modelo_beta_probit_plus <- betareg(formula = WINP ~ PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_pts <- betareg(formula = WINP ~ PTS + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_fga <- betareg(formula = WINP ~ FGA + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_fgm <- betareg(formula = WINP ~ FGM + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_fgp <- betareg(formula = WINP ~ FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_3pa <- betareg(formula = WINP ~ `3PA` + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_3pm <- betareg(formula = WINP ~ `3PM` + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_3pp <- betareg(formula = WINP ~ `3PP` + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_fta <- betareg(formula = WINP ~ FGP + FTA + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_ftm <- betareg(formula = WINP ~ FGP + FTM + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_ftp <- betareg(formula = WINP ~ FGP + FTP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_reb <- betareg(formula = WINP ~ REB + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_oreb <- betareg(formula = WINP ~ OREB + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_dreb <- betareg(formula = WINP ~ DREB + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_ast <- betareg(formula = WINP ~ AST + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_tov <- betareg(formula = WINP ~ TOV + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_stl <- betareg(formula = WINP ~ STL + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_blk <- betareg(formula = WINP ~  BLK +FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_blka <- betareg(formula = WINP ~ BLKA + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_pf <- betareg(formula = WINP ~ PF + FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_pfd <- betareg(formula = WINP ~ PFD +FGP + PlusMinus, data = dados_regressao, link = "probit")
modelo_beta_probit_team <- betareg(formula = WINP ~ TEAM + PF+ FGP + PlusMinus, data = dados_regressao, link = "probit")

lrtest(modelo_beta_probit_vazio, modelo_beta_probit_plus) #2.2e-16, plusminus significativo
lrtest(modelo_beta_probit_plus, modelo_beta_probit_pts) #PTS não significativo
lrtest(modelo_beta_probit_plus, modelo_beta_probit_fga) #FGA não significativo
lrtest(modelo_beta_probit_plus, modelo_beta_probit_fgm) #0.8765, FGM não significativo
lrtest(modelo_beta_probit_plus, modelo_beta_probit_fgp) #FGP  significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_3pa) #3PA  não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_3pm) #3PM  não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_3pp) #3PP  não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_fta) #FTA  não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_ftm) #FTM  não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_ftp) #FTP não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_reb) #REB não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_dreb) #DREB não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_oreb) #OREB não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_ast) #AST não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_tov) #TOV não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_stl) #STL não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_blk) #blk não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_blka) #blka não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_pfd) #pfd não significativo
lrtest(modelo_beta_probit_fgp, modelo_beta_probit_pf) #pf significativo
lrtest(modelo_beta_probit_pf, modelo_beta_probit_team) #team significativo

#Melhor modelo de probito é modelo_beta_probit_team com TEAM + PF + FGP + PlusMinus

#######cloglog####### 
modelo_betac_vazio <- betareg(formula = WINP ~ 1, data = dados_regressao, link = "cloglog")
modelo_betac_plus <- betareg(formula = WINP ~ PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_pts <- betareg(formula = WINP ~ PTS + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_fga <- betareg(formula = WINP ~ FGA + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_fgm <- betareg(formula = WINP ~ FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_fgp <- betareg(formula = WINP ~ FGA + FGM + FGP + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_3pa <- betareg(formula = WINP ~ `3PA` + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_3pm <- betareg(formula = WINP ~ `3PM` + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_3pp <- betareg(formula = WINP ~ `3PP` + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_fta <- betareg(formula = WINP ~ FGA + FGM + FTA + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_ftm <- betareg(formula = WINP ~ FGA + FGM + FTM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_ftp <- betareg(formula = WINP ~ FGA + FGM + FTP + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_reb <- betareg(formula = WINP ~ REB + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_oreb <- betareg(formula = WINP ~ OREB + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_dreb <- betareg(formula = WINP ~ DREB + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_ast <- betareg(formula = WINP ~ AST + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_tov <- betareg(formula = WINP ~ TOV + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_stl <- betareg(formula = WINP ~ STL + TOV + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_blk <- betareg(formula = WINP ~  BLK + TOV + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_blka <- betareg(formula = WINP ~ BLKA + TOV +  FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_pf <- betareg(formula = WINP ~ PF + TOV + FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_pfd <- betareg(formula = WINP ~ PFD + TOV +  FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")
modelo_betac_team <- betareg(formula = WINP ~ TEAM + TOV +  FGA + FGM + PlusMinus, data = dados_regressao, link = "cloglog")

lrtest(modelo_betac_vazio, modelo_betac_plus) #2.2e-16, plusminus significativo
lrtest(modelo_betac_plus, modelo_betac_pts) #PTS não significativo
lrtest(modelo_betac_plus, modelo_betac_fga) #FGA significativo
lrtest(modelo_betac_fga, modelo_betac_fgm) #FGM significativo
lrtest(modelo_betac_fgm, modelo_betac_fgp) #FGP não significativo
lrtest(modelo_betac_fgm, modelo_betac_3pa) #3PA  não significativo
lrtest(modelo_betac_fgm, modelo_betac_3pm) #3PM  não significativo
lrtest(modelo_betac_fgm, modelo_betac_3pp) #3PP  não significativo
lrtest(modelo_betac_fgm, modelo_betac_fta) #FTA  não significativo
lrtest(modelo_betac_fgm, modelo_betac_ftm) #FTM  não significativo
lrtest(modelo_betac_fgm, modelo_betac_ftp) #FTP não significativo
lrtest(modelo_betac_fgm, modelo_betac_reb) #REB não significativo
lrtest(modelo_betac_fgm, modelo_betac_dreb) #DREB não significativo
lrtest(modelo_betac_fgm, modelo_betac_oreb) #OREB não significativo
lrtest(modelo_betac_fgm, modelo_betac_ast) #AST não significativo
lrtest(modelo_betac_fgm, modelo_betac_tov) #TOV significativo
lrtest(modelo_betac_tov, modelo_betac_stl) #STL não significativo
lrtest(modelo_betac_tov, modelo_betac_blk) #blk não significativo
lrtest(modelo_betac_tov, modelo_betac_blka) #blka não significativo
lrtest(modelo_betac_tov, modelo_betac_pfd) #pfd não significativo
lrtest(modelo_betac_tov, modelo_betac_pf) #pf não significativo
lrtest(modelo_betac_pf, modelo_betac_team) #team significativo

#Melhor modelo é com TEAM + TOV + FGA + FGM + PlusMinus

####### cauchito ####
#######cloglog####### 
modelo_beta_cauchit_vazio <- betareg(formula = WINP ~ 1, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_plus <- betareg(formula = WINP ~ PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_pts <- betareg(formula = WINP ~ PTS + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_fga <- betareg(formula = WINP ~ FGA + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_fgm <- betareg(formula = WINP ~ FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_fgp <- betareg(formula = WINP ~ FGA + FGM + FGP + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_3pa <- betareg(formula = WINP ~ `3PA` + FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_3pm <- betareg(formula = WINP ~ `3PM` + FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_3pp <- betareg(formula = WINP ~ `3PP` + FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_fta <- betareg(formula = WINP ~ FGA + FGM + FTA + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_ftm <- betareg(formula = WINP ~ FGA + FGM + FTM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_ftp <- betareg(formula = WINP ~ FGA + FGM + FTP + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_reb <- betareg(formula = WINP ~ REB + FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_oreb <- betareg(formula = WINP ~ OREB + FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_dreb <- betareg(formula = WINP ~ DREB + FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_ast <- betareg(formula = WINP ~ AST + FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_tov <- betareg(formula = WINP ~ TOV + FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_stl <- betareg(formula = WINP ~ STL +  FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_blk <- betareg(formula = WINP ~  BLK +  FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_blka <- betareg(formula = WINP ~ BLKA +  FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_pf <- betareg(formula = WINP ~ PF +  FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_pfd <- betareg(formula = WINP ~ PFD +  FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")
modelo_beta_cauchit_team <- betareg(formula = WINP ~ TEAM + PF + FGA + FGM + PlusMinus, data = dados_regressao, link = "cauchit")

lrtest(modelo_beta_cauchit_vazio, modelo_beta_cauchit_plus) #2.2e-16, plusminus significativo
lrtest(modelo_beta_cauchit_plus, modelo_beta_cauchit_pts) #PTS não significativo
lrtest(modelo_beta_cauchit_plus, modelo_beta_cauchit_fga) #FGA significativo
lrtest(modelo_beta_cauchit_fga, modelo_beta_cauchit_fgm) #FGM significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_fgp) #FGP não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_3pa) #3PA  não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_3pm) #3PM  não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_3pp) #3PP  não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_fta) #FTA  não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_ftm) #FTM  não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_ftp) #FTP não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_reb) #REB não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_dreb) #DREB não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_oreb) #OREB não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_ast) #AST não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_tov) #TOV não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_stl) #STL não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_blk) #blk não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_blka) #blka não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_pfd) #pfd não significativo
lrtest(modelo_beta_cauchit_fgm, modelo_beta_cauchit_pf) #pf significativo
lrtest(modelo_beta_cauchit_pf, modelo_beta_cauchit_team) #team não significativo

#Melhor modelo modelo_beta_cauchit_pf com  PF + FGA + FGM + PlusMinus
############## Análise de resíduos apenas dos melhores modelos por função de ligação ###################
########### Logito ##########
#### Modelo completo logito ###
plot(modelo_beta1, which = 1, type = "pearson") #Residuos vs indices das observações
plot(modelo_beta1, which = 2, type = "pearson") #Distância de cook
plot(modelo_beta1, which = 3, type = "pearson") #alavancagem vs valores preditos
plot(modelo_beta1, which = 4, type = "pearson") #Residuos vs preditores lineares
plot(modelo_beta1, which = 5, type = "deviance", sub.caption = "") #QQ plot
plot(modelo_beta1, which = 1, type = "deviance", sub.caption = "") #Residuos vs indices das observações
shapiro.test(modelo_beta1$residuals) #p-value = 0.3693, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta1) #p-value = 0.1306
#Independência
plot(modelo_beta1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta1$fitted.values, modelo_beta1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta1) #p-value = 0.2463, heterocedasticidade

#### Modelo reduzido logito ###
plot(modelo_beta11, which = 1, type = "pearson")
plot(modelo_beta11, which = 2, type = "pearson")
plot(modelo_beta11, which = 3, type = "pearson")
plot(modelo_beta11, which = 4, type = "pearson")
plot(modelo_beta11, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta11, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta11$residuals) #p-value = 0.5895, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta11) #p-value = 0.2889
#Independência
plot(modelo_beta1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta1$fitted.values, modelo_beta1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta11) #p-value = 0.03674, heterocedasticidade

### Modelo 10% logito ###
plot(modelo_beta12, which = 1, type = "pearson")
plot(modelo_beta12, which = 2, type = "pearson")
plot(modelo_beta12, which = 3, type = "pearson")
plot(modelo_beta12, which = 4, type = "pearson")
plot(modelo_beta12, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta12, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta12$residuals) #p-value = 0.5895, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta11) #p-value = 0.2889
#Independência
plot(modelo_beta1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta1$fitted.values, modelo_beta1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta11) #p-value = 0.03674, heterocedasticidade

########### Loglog ########
### Modelo completo log log ###
plot(modelo_beta2, which = 1, type = "pearson")
plot(modelo_beta2, which = 2, type = "pearson")
plot(modelo_beta2, which = 3, type = "pearson")
plot(modelo_beta2, which = 4, type = "pearson")
plot(modelo_beta2, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta2, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta2$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta2) #p-value = 
#Independência
plot(modelo_beta2$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta2$fitted.values, modelo_beta2$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta2) #p-value = 

### Modelo 5% loglog ###
plot(modelo_beta21, which = 1, type = "pearson")
plot(modelo_beta21, which = 2, type = "pearson")
plot(modelo_beta21, which = 3, type = "pearson")
plot(modelo_beta21, which = 4, type = "pearson")
plot(modelo_beta21, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta21, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta21$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta21) #p-value = 
#Independência
plot(modelo_beta21$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta21$fitted.values, modelo_beta21$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta21) #p-value = 

### Modelo 10% loglog ###
plot(modelo_beta22, which = 1, type = "pearson")
plot(modelo_beta22, which = 2, type = "pearson")
plot(modelo_beta22, which = 3, type = "pearson")
plot(modelo_beta22, which = 4, type = "pearson")
plot(modelo_beta22, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta22, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta21$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta22) #p-value = 
#Independência
plot(modelo_beta22$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta22$fitted.values, modelo_beta22$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta22) #p-value = 

########## Modelos Probito ######
### Modelo completo probito ###
plot(modelo_beta_probit, which = 1, type = "pearson")
plot(modelo_beta_probit, which = 2, type = "pearson")
plot(modelo_beta_probit, which = 3, type = "pearson")
plot(modelo_beta_probit, which = 4, type = "pearson")
plot(modelo_beta_probit, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta_probit, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta_probit$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta_probit) #p-value = 
#Independência
plot(modelo_beta_probit$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta_probit$fitted.values, modelo_beta_probit$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta_probit) #p-value = 

### Modelo 5% probito ###
plot(modelo_beta_probit1, which = 1, type = "pearson")
plot(modelo_beta_probit1, which = 2, type = "pearson")
plot(modelo_beta_probit1, which = 3, type = "pearson")
plot(modelo_beta_probit1, which = 4, type = "pearson")
plot(modelo_beta_probit1, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta_probit1, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta_probit1$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta_probit1) #p-value = 
#Independência
plot(modelo_beta_probit1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta_probit1$fitted.values, modelo_beta_probit1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta_probit1) #p-value = 

### Modelo 10% probito ###
plot(modelo_beta_probit2, which = 1, type = "pearson")
plot(modelo_beta_probit2, which = 2, type = "pearson")
plot(modelo_beta_probit2, which = 3, type = "pearson")
plot(modelo_beta_probit2, which = 4, type = "pearson")
plot(modelo_beta_probit2, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta_probit2, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta_probit2$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta_probit2) #p-value = 
#Independência
plot(modelo_beta_probit2$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta_probit2$fitted.values, modelo_beta_probit2$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta_probit2) #p-value = 

############ Cloglog ##############
### Modelo completo cloglog ###
plot(modelo_beta_cloglog, which = 1, type = "pearson")
plot(modelo_beta_cloglog, which = 2, type = "pearson")
plot(modelo_beta_cloglog, which = 3, type = "pearson")
plot(modelo_beta_cloglog, which = 4, type = "pearson")
plot(modelo_beta_cloglog, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta_cloglog, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta_cloglog$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta_cloglog) #p-value = 
#Independência
plot(modelo_beta_cloglog$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta_cloglog$fitted.values, modelo_beta_cloglog$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta_cloglog) #p-value = 

### Modelo 5% cloglog ###
plot(modelo_beta_cloglog1, which = 1, type = "pearson")
plot(modelo_beta_cloglog1, which = 2, type = "pearson")
plot(modelo_beta_cloglog1, which = 3, type = "pearson")
plot(modelo_beta_cloglog1, which = 4, type = "pearson")
plot(modelo_beta_cloglog1, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta_cloglog1, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta_cloglog1$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta_cloglog1) #p-value = 
#Independência
plot(modelo_beta_cloglog1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta_cloglog1$fitted.values, modelo_beta_cloglog1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta_cloglog1) #p-value =

### Modelo 10% cloglog ###
plot(modelo_beta_cloglog2, which = 1, type = "pearson")
plot(modelo_beta_cloglog2, which = 2, type = "pearson")
plot(modelo_beta_cloglog2, which = 3, type = "pearson")
plot(modelo_beta_cloglog2, which = 4, type = "pearson")
plot(modelo_beta_cloglog2, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta_cloglog2, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta_cloglog2$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta_cloglog2) #p-value = 
#Independência
plot(modelo_beta_cloglog2$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta_cloglog2$fitted.values, modelo_beta_cloglog2$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta_cloglog2) #p-value =

############# Cauchit #######################
### Modelo completo cauchit ###
plot(modelo_beta_cauchit, which = 1, type = "pearson")
plot(modelo_beta_cauchit, which = 2, type = "pearson")
plot(modelo_beta_cauchit, which = 3, type = "pearson")
plot(modelo_beta_cauchit, which = 4, type = "pearson")
plot(modelo_beta_cauchit, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta_cauchit, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta_cauchit$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta_cauchit) #p-value = 
#Independência
plot(modelo_beta_cauchit$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta_cauchit$fitted.values, modelo_beta_cauchit$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta_cauchit) #p-value =

### Modelo 5% cauchit ###
plot(modelo_beta_cauchit1, which = 1, type = "pearson")
plot(modelo_beta_cauchit1, which = 2, type = "pearson")
plot(modelo_beta_cauchit1, which = 3, type = "pearson")
plot(modelo_beta_cauchit1, which = 4, type = "pearson")
plot(modelo_beta_cauchit1, which = 5, type = "deviance", sub.caption = "")
plot(modelo_beta_cauchit1, which = 1, type = "deviance", sub.caption = "")
shapiro.test(modelo_beta_cauchit1$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_beta_cauchit1) #p-value = 
#Independência
plot(modelo_beta_cauchit1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_beta_cauchit1$fitted.values, modelo_beta_cauchit1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_beta_cauchit1) #p-value =