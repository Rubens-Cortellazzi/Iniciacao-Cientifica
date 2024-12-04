source("dados_playoffs.R")

############## Regressão Beta ########

#Da para fazer a comparação entre loglog e probito pois são os maiores valores de 
#Pseudo R-squared entre as funções de ligação.

## Fazer transformação (y* (n - 1) + 0.5)/n porque contém as extremidades
# Transformação que Smithson e Verkuiken (2006) indicaram no livro.

n = count(dados_regressaop) # n = 240
#Testanto a transformação
#WINP_transformado = (dados_regressaop$WIN_P*(240 - 1) + 0.5)/240

playoffs_transformado <- dados_regressaop %>% 
  mutate(WINP_transformado = (dados_regressaop$WINP*(240 - 1) + 0.5)/240) %>%
  dplyr::select(-WINP)

playoffs_transformado
summary(playoffs_transformado$WINP_transformado) # Agora o minímo não é mais 0.

##Leitura do pacote
library(betareg)

########## Regresão beta sem dados transformados. ######

#Está dando erro, porque diz que tem que estar entre (0, 1), e no banco de dados tem
#observações na WIN_P com 0 vitórias.
# modelo_betap_s1 <- betareg(WINP ~ . ,data = dados_regressaop) #Regressão com todos os dados do modelo
# modelo_betap_s1
# summary(modelo_betap_s1)
# coef(modelo_betap_s1)
# car::Anova(modelo_betap_s1)
# modelo_betap_s2 <- betareg(WIN_P ~ .,data = dados_regressaop, link = "loglog") #Regressão com todos os dados do modelo
# modelo_betap_s2
# summary(modelo_betap_s2)
# coef(modelo_betap_s2)

###Regressão beta com os dados transformados, segundo a transformação que citei anteriormente ########
#### Logito ########

##### modelo completo
modelo_betapt1 <- betareg(WINP_transformado ~ . ,data = playoffs_transformado) #Regressão com todos os dados do modelo
modelo_betapt1
coef(modelo_betapt1)
car::Anova(modelo_betapt1)

#### Modelo com as variáveis mais significantes com 5% 
modelo_betapt11 <- betareg(WINP_transformado ~ TEAM + PTS + FGM + `3PM` + FTM + FTA + FTP + OREB + DREB + REB + PlusMinus,data = playoffs_transformado) 
modelo_betapt11
coef(modelo_betapt11)
car::Anova(modelo_betapt11)
#####Fazendo a regressão beta, mas com loglog ####

#####Com todas as variáveis do modelo
modelo_betat_loglog <- betareg(WINP_transformado ~ .,data = playoffs_transformado, link = "loglog") #Regressão com todos os dados do modelo
modelo_betat_loglog
car::Anova(modelo_betat_loglog) #Pseudo R-squared: 0.7097
coef(modelo_betat_loglog)

###### com variáveis significantes com alfa = 5%
modelo_betat_loglog1 <- betareg(WINP_transformado ~ TEAM + PTS + FGM + `3PM` + FTM + FTA + FTP + OREB + DREB + REB + PlusMinus,data = playoffs_transformado, link = "loglog") #Regressão com todos os dados do modelo
modelo_betat_loglog1
car::Anova(modelo_betat_loglog1) #FTA e FTP não significantes
coef(modelo_betat_loglog1)

##### Fazendo a regressão beta, mas com probito #######

#Modelo completo
modelo_betat_probit <- betareg(WINP_transformado ~ .,data = playoffs_transformado, link = "probit")
modelo_betat_probit
coef(modelo_betat_probit)
car::Anova(modelo_betat_probit)

#Modelo com 5%
modelo_betat_probit2 <- betareg(WINP_transformado ~ TEAM + PTS + FGM + `3PM` + FTM + FTA + FTP + OREB + DREB + REB + PlusMinus,data = playoffs_transformado, link = "probit")
modelo_betat_probit2
summary(modelo_betat_probit2) #Pseudo R-squared: 0.638
coef(modelo_betat_probit2)

#####Fazendo a regressão beta, mas com cloglog #######

#Modelo completo
modelo_betat_cloglog <- betareg(WINP_transformado ~ ., data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog
coef(modelo_betat_cloglog)
car::Anova(modelo_betat_cloglog)

#Modelo com 5%
modelo_betat_cloglog1 <- betareg(WINP_transformado ~ TEAM + PTS + FGM + `3PM` + FTM + FTP + OREB + DREB + REB + PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog1
car::Anova(modelo_betat_cloglog1) #FTP não significativo
coef(modelo_betat_cloglog1)

#####Fazendo a regressão beta, mas com cauchito ######

#Modelo completo
modelo_betat_cauchit <- betareg(WINP_transformado ~ ., data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit
coef(modelo_betat_cauchit)
car::Anova(modelo_betat_cauchit)

#Modelo com significância de 10%
modelo_betat_cauchit2 <- betareg(WINP_transformado ~ PTS + FGM + `3PM` + FTM + FTA + FTP + OREB + REB + PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit2
summary(modelo_betat_cauchit2) #Pseudo R-squared: 0.3149
coef(modelo_betat_cauchit2)

######## Anova ########
##### Logito ####
modelo_betapt1
modelo_betapt11 #TEAM + PTS + FGM + `3PM` + FTM + FTA + FTP + OREB + DREB + REB + PlusMinus
modelo_betapt_vazio <- betareg(WINP_transformado ~ 1 ,data = playoffs_transformado) 
modelo_betapt_plus <- betareg(WINP_transformado ~ PlusMinus ,data = playoffs_transformado)
modelo_betapt_reb <- betareg(WINP_transformado ~ REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_dreb <- betareg(WINP_transformado ~ DREB + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_oreb <- betareg(WINP_transformado ~ OREB + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_ftm <- betareg(WINP_transformado ~ FTM + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_3pm <- betareg(WINP_transformado ~ `3PM` + REB + PlusMinus ,data = playoffs_transformado)
modelo_betapt_fgm <- betareg(WINP_transformado ~ FGM + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_pts <- betareg(WINP_transformado ~ PTS + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_ftp <- betareg(WINP_transformado ~ FTP + REB + PlusMinus ,data = playoffs_transformado)
modelo_betapt_fta <- betareg(WINP_transformado ~ FTA + FTP + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_pf <- betareg(WINP_transformado ~ PF + FTP + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_pfd <- betareg(WINP_transformado ~ PFD + FTP + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_blk <- betareg(WINP_transformado ~ BLK + FTP + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_blka <- betareg(WINP_transformado ~ BLKA + FTP + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_ast <- betareg(WINP_transformado ~ AST + FTP + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_tov <- betareg(WINP_transformado ~ TOV + FTP + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_stl <- betareg(WINP_transformado ~ STL + FTP + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_fga <- betareg(WINP_transformado ~ FGA + FTP + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_fgp <- betareg(WINP_transformado ~ FGP + FTP + REB + PlusMinus ,data = playoffs_transformado)
modelo_betapt_3pa <- betareg(WINP_transformado ~ `3PA` + FTP + REB + PlusMinus ,data = playoffs_transformado) 
modelo_betapt_3pp <- betareg(WINP_transformado ~ `3PP` + FTP + REB + PlusMinus ,data = playoffs_transformado)
modelo_betapt_team <- betareg(WINP_transformado ~ TEAM + FTP + REB + PlusMinus ,data = playoffs_transformado)

lrtest(modelo_betapt_vazio, modelo_betapt_plus) #PlusMinus significante
lrtest(modelo_betapt_plus, modelo_betapt_reb)#0.056 REB foi significativo
lrtest(modelo_betapt_reb, modelo_betapt_dreb) #0.4961, DREB não foi significativo
lrtest(modelo_betapt_reb, modelo_betapt_oreb) #0.4356, OREB não foi significativo
lrtest(modelo_betapt_reb, modelo_betapt_ftm) #0.5347, FTM não foi significativo
lrtest(modelo_betapt_reb, modelo_betapt_3pm) #0.5708, 3PM não foi significativo
lrtest(modelo_betapt_reb, modelo_betapt_fgm) #0.5481, FGM não foi significativo
lrtest(modelo_betapt_reb, modelo_betapt_pts) #0.6963, PTS não foi significativo
lrtest(modelo_betapt_reb, modelo_betapt_ftp) #0.05651, FTP  foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_fta) #0.7973, FTA  não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_pf) #PF  não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_pfd) #PFD  não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_blk) #blk  não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_blka) #blka  não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_ast) #ast  não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_tov) #tov não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_stl) #stl não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_fga) #fga não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_fgp) #fgp não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_3pa) #3pa não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_3pp) #3pp não foi significativo
lrtest(modelo_betapt_ftp, modelo_betapt_team) #team foi significativo


#Melhor modelo foi modelo_betapt_team que contém TEAM + FTP + REB + PlusMinus
##### loglog #####
modelo_betat_loglog
modelo_betat_loglog1 #TEAM + PTS + FGM + `3PM` + FTM + FTA + FTP + OREB + DREB + REB + PlusMinus
modelop_loglog_vazio <- betareg(formula = WINP_transformado ~ 1, data = playoffs_transformado, link = "loglog")
modelop_loglog_plus <- betareg(formula = WINP_transformado ~ PlusMinus, data = playoffs_transformado, link = "loglog")
modelop_loglog_reb <- betareg(formula = WINP_transformado ~ REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_dreb <- betareg(formula = WINP_transformado ~ DREB + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_oreb <- betareg(formula = WINP_transformado ~ OREB + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_ftm <- betareg(formula = WINP_transformado ~ FTM + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_fta <- betareg(formula = WINP_transformado ~ FTA + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_ftp <- betareg(formula = WINP_transformado ~ FTP + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_3pm <- betareg(formula = WINP_transformado ~ `3PM` + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_3pa <- betareg(formula = WINP_transformado ~ `3PA` + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_3pp <- betareg(formula = WINP_transformado ~ `3PP` + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_fgm <- betareg(formula = WINP_transformado ~ FGM + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_fga <- betareg(formula = WINP_transformado ~ FGA + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_fgp <- betareg(formula = WINP_transformado ~ FGP + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_pts <- betareg(formula = WINP_transformado ~ PTS + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_ast <- betareg(formula = WINP_transformado ~ AST + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_tov <- betareg(formula = WINP_transformado ~ TOV + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_stl <- betareg(formula = WINP_transformado ~ STL + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_blk <- betareg(formula = WINP_transformado ~ BLK + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_blka <- betareg(formula = WINP_transformado ~ BLKA + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_pf <- betareg(formula = WINP_transformado ~ PF + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_pfd <- betareg(formula = WINP_transformado ~ PFD + REB + PlusMinus,data = playoffs_transformado, link = "loglog")
modelop_loglog_team <- betareg(formula = WINP_transformado ~ TEAM + REB + PlusMinus,data = playoffs_transformado, link = "loglog")

lrtest(modelop_loglog_vazio, modelop_loglog_plus)#Plus Minus foi significativo
lrtest(modelop_loglog_plus, modelop_loglog_reb)#0.006634 REB foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_dreb)#0.4797 DREB não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_oreb)#0.4129 OREB não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_oreb)#0.4129 OREB não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_ftm)#0.7688 FTM não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_fta)#FTA não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_ftp)#FTP não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_3pa)#3PA não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_3pp)#3PP não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_3pm)#0.8251 3PM não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_fgm)#0.6389 FGM não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_fga)#FGA não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_fgp)#FGP não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_pts)#0.7887 PTS não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_ast)#AST não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_tov)#Tov não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_stl)#stl não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_blk)#blk não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_blka)#blka não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_pf)#pf não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_pfd)#PFD não foi significativo
lrtest(modelop_loglog_reb, modelop_loglog_team)#TEAM foi significativo

#Melhor modelo foi o modelop_loglog_team com REB + PlusMinus + TEAM
##### Probito ####
modelo_betat_probit
modelo_betat_probit2 #PTS + FGM + `3PM` + FTM + FTA + FTP + OREB +DREB + REB + PlusMinus
modelop_probit_vazio <- betareg(formula = WINP_transformado ~ 1, data = playoffs_transformado, link = "probit")
modelop_probit_plus <- betareg(formula = WINP_transformado ~ PlusMinus, data = playoffs_transformado, link = "probit")
modelop_probit_reb <- betareg(formula = WINP_transformado ~ REB + PlusMinus, data = playoffs_transformado, link = "probit")
modelop_probit_dreb <- betareg(formula = WINP_transformado ~ DREB + REB + PlusMinus, data = playoffs_transformado, link = "probit")
modelop_probit_oreb <- betareg(formula = WINP_transformado ~ OREB + REB + PlusMinus, data = playoffs_transformado, link = "probit")
modelop_probit_ftp <- betareg(formula = WINP_transformado ~ FTP + REB + PlusMinus, data = playoffs_transformado, link = "probit")
modelop_probit_fta <- betareg(formula = WINP_transformado ~ FTA + FTP + REB + PlusMinus, data = playoffs_transformado, link = "probit")
modelop_probit_ftm <- betareg(formula = WINP_transformado ~ FTM + FTP + REB + PlusMinus, data = playoffs_transformado, link = "probit")
modelop_probit_3pm <- betareg(formula = WINP_transformado ~ `3PM` + FTP + REB + PlusMinus, data = playoffs_transformado, link = "probit")
modelop_probit_fgm <- betareg(formula = WINP_transformado ~ FGM + FTP + REB + PlusMinus, data = playoffs_transformado, link = "probit")
modelop_probit_pts <- betareg(formula = WINP_transformado ~ PTS + FTP + REB + PlusMinus, data = playoffs_transformado, link = "probit")
modelop_probit_team <- betareg(formula = WINP_transformado ~ TEAM + FTP + REB + PlusMinus, data = playoffs_transformado, link = "probit")

lrtest(modelop_probit_vazio, modelop_probit_plus) #PlusMinus significativo
lrtest(modelop_probit_plus, modelop_probit_reb) #0.03236,  REB deu significativo
lrtest(modelop_probit_reb, modelop_probit_dreb) #0.5235,  DREB deu não significativo
lrtest(modelop_probit_reb, modelop_probit_oreb) # 0.4588,  OREB deu não significativo
lrtest(modelop_probit_reb, modelop_probit_ftp) # 0.0691,  FTP deu significativo
lrtest(modelop_probit_ftp, modelop_probit_fta) #0.834, FTA deu não significativo
lrtest(modelop_probit_ftp, modelop_probit_ftm) #0.9295, FTM deu não significativo
lrtest(modelop_probit_ftp, modelop_probit_3pm) #0.4011, 3PM deu não significativo
lrtest(modelop_probit_ftp, modelop_probit_fgm) #0.4889, FGM deu não significativo
lrtest(modelop_probit_ftp, modelop_probit_pts) #0.4339, PTS deu não significativo
lrtest(modelop_probit_ftp, modelop_probit_team) #TEAM significativo

#Melhor modelo é modelop_probit_ftp com FTP + REB + PlusMinus + TEAM
##### cloglog ####
modelo_betat_cloglog
modelo_betat_cloglog1 #TEAM + PTS + FGM + `3PM` + FTM + FTP + OREB + DREB + REB + PlusMinus
modelo_betat_cloglog_plus <- betareg(formula = WINP_transformado ~ PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog_reb <- betareg(formula = WINP_transformado ~ REB + PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog_dreb <- betareg(formula = WINP_transformado ~ DREB + REB + PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog_oreb <- betareg(formula = WINP_transformado ~ OREB + REB + PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog_ftp <- betareg(formula = WINP_transformado ~ FTP + REB + PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog_fta <- betareg(formula = WINP_transformado ~ FTA + FTP + REB + PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog_ftm <- betareg(formula = WINP_transformado ~ FTM + FTP + REB + PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog_3pm <- betareg(formula = WINP_transformado ~ `3PM` + FTP + REB + PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog_fgm <- betareg(formula = WINP_transformado ~ FGM + FTP + REB + PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog_pts <- betareg(formula = WINP_transformado ~ PTS + FTP + REB + PlusMinus, data = playoffs_transformado, link = "cloglog")
modelo_betat_cloglog_team <- betareg(formula = WINP_transformado ~ TEAM + FTP + REB + PlusMinus, data = playoffs_transformado, link = "cloglog")

lrtest(modelop_probit_plus, modelo_betat_cloglog_reb) #0.004715,  REB deu significativo
lrtest(modelo_betat_cloglog_reb, modelo_betat_cloglog_dreb) #0.7577,  DREB deu não significativo
lrtest(modelo_betat_cloglog_reb, modelo_betat_cloglog_oreb) # 0.6921,  OREB deu não significativo
lrtest(modelo_betat_cloglog_reb, modelo_betat_cloglog_ftp) # 0.04811,  FTP deu significativo
lrtest(modelo_betat_cloglog_ftp, modelo_betat_cloglog_fta) #0.6894, FTA deu não significativo
lrtest(modelo_betat_cloglog_ftp, modelo_betat_cloglog_ftm) #0.7763, FTM deu não significativo
lrtest(modelo_betat_cloglog_ftp, modelo_betat_cloglog_3pm) #0.2797, 3PM deu não significativo
lrtest(modelo_betat_cloglog_ftp, modelo_betat_cloglog_fgm) #0.3304, FGM deu não significativo
lrtest(modelo_betat_cloglog_ftp, modelo_betat_cloglog_pts) #0.3, PTS deu não significativo
lrtest(modelo_betat_cloglog_ftp, modelo_betat_cloglog_team) #TEAM significativo

#modelo_betat_cloglog_ftp foi o melhor modelo com FTP + REB + PlusMinus + TEAM

##### Cauchito ####
modelo_betat_cauchit
modelo_betat_cauchit2 #PTS + FGM + `3PM` + FTM + FTA + FTP + OREB + REB + PlusMinus
modelo_betat_cauchit_plus <- betareg(formula = WINP_transformado ~ PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit_reb <- betareg(formula = WINP_transformado ~ REB + PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit_dreb <- betareg(formula = WINP_transformado ~ DREB + PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit_oreb <- betareg(formula = WINP_transformado ~ OREB + PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit_ftp <- betareg(formula = WINP_transformado ~ FTP + PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit_fta <- betareg(formula = WINP_transformado ~ FTA + FTP + PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit_ftm <- betareg(formula = WINP_transformado ~ FTM + FTP + PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit_3pm <- betareg(formula = WINP_transformado ~ `3PM` + FTP + PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit_fgm <- betareg(formula = WINP_transformado ~ FGM + FTP + PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit_pts <- betareg(formula = WINP_transformado ~ PTS + FTP +PlusMinus, data = playoffs_transformado, link = "cauchit")
modelo_betat_cauchit_team <- betareg(formula = WINP_transformado ~ TEAM + FTP +PlusMinus, data = playoffs_transformado, link = "cauchit")

lrtest(modelo_betat_cauchit_plus, modelo_betat_cauchit_reb) #0.2505,  REB deu não significativo
lrtest(modelo_betat_cauchit_plus, modelo_betat_cauchit_dreb) #0.1528,  DREB deu não significativo
lrtest(modelo_betat_cauchit_plus, modelo_betat_cauchit_oreb) # 0.9832,  OREB deu não significativo
lrtest(modelo_betat_cauchit_plus, modelo_betat_cauchit_ftp) # 0.08719,  FTP deu significativo
lrtest(modelo_betat_cauchit_ftp, modelo_betat_cauchit_fta) #0.7643, FTA deu não significativo
lrtest(modelo_betat_cauchit_ftp, modelo_betat_cauchit_ftm) #0.7763, FTM deu não significativo
lrtest(modelo_betat_cauchit_ftp, modelo_betat_cauchit_3pm) #0.3074, 3PM deu não significativo
lrtest(modelo_betat_cauchit_ftp, modelo_betat_cauchit_fgm) #0.8358, FGM deu não significativo
lrtest(modelo_betat_cauchit_ftp, modelo_betat_cauchit_pts) #0.6404, PTS deu não significativo
lrtest(modelo_betat_cauchit_ftp, modelo_betat_cauchit_team) #TEAM significativo

#Melhor modelo modelo_betat_cauchit_team com FTP + PlusMinus + TEAM
######## Análise de resíduos ########

##### Logito #######
#### Modelo 1 ####
shapiro.test(modelo_betapt1$residuals) #p-value = 0.7859, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betapt1) #p-value = 0.1243
#Independência
plot(modelo_betapt1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betapt1$fitted.values, modelo_betapt1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betapt1) #p-value = 0.004251, heterocedasticidade

#### Modelo 11 ####
plot(modelo_betapt11, which = 1)
plot(modelo_betapt11, which = 2)
plot(modelo_betapt11, which = 3)
plot(modelo_betapt11, which = 4)
plot(modelo_betapt11, which = 5)
plot(modelo_betapt11, which = 6)
shapiro.test(modelo_betapt11$residuals) #p-value = 0.7859, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betapt11) #p-value = 0.1243
#Independência
plot(modelo_betapt11$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betapt11$fitted.values, modelo_betapt11$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betapt11) #p-value = 0.004251, heterocedasticidade

#Modelo 12 ####
plot(modelo_betapt12, which = 1)
plot(modelo_betapt12, which = 2)
plot(modelo_betapt12, which = 3)
plot(modelo_betapt12, which = 4)
plot(modelo_betapt12, which = 5)
plot(modelo_betapt12, which = 6)
shapiro.test(modelo_betapt12$residuals) #p-value = 0.6838, normal
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betapt12) #p-value = 0.04034
#Independência
plot(modelo_betapt12$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betapt12$fitted.values, modelo_betapt12$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betapt12) #p-value = 0.0007128, heterocedasticidade

#Modelo 13 ####
plot(modelo_betapt13, which = 1)
plot(modelo_betapt13, which = 2)
plot(modelo_betapt13, which = 3)
plot(modelo_betapt13, which = 4)
plot(modelo_betapt13, which = 5)
plot(modelo_betapt13, which = 6)
shapiro.test(modelo_betapt13$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betapt13) #p-value = 0.04034
#Independência
plot(modelo_betapt13$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betapt13$fitted.values, modelo_betapt13$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betapt13) #p-value = 0.0007128, heterocedasticidade

############ loglog #########
####### Modelo completo ####
plot(modelo_betat_loglog, which = 1)
plot(modelo_betat_loglog, which = 2)
plot(modelo_betat_loglog, which = 3)
plot(modelo_betat_loglog, which = 4)
plot(modelo_betat_loglog, which = 6)
shapiro.test(modelo_betat_loglog$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_loglog) #p-value = 0.04034
#Independência
plot(modelo_betat_loglog$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_loglog$fitted.values, modelo_betat_loglog$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_loglog) #p-value = 0.0007128, heterocedasticidade

####### Modelo 5% ####
plot(modelo_betat_loglog1, which = 1)
plot(modelo_betat_loglog1, which = 2)
plot(modelo_betat_loglog1, which = 3)
plot(modelo_betat_loglog1, which = 4)
plot(modelo_betat_loglog1, which = 5)
plot(modelo_betat_loglog1, which = 6)
shapiro.test(modelo_betat_loglog1$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_loglog1) #p-value = 0.04034
#Independência
plot(modelo_betat_loglog1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_loglog1$fitted.values, modelo_betat_loglog1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_loglog1) #p-value = 0.0007128, heterocedasticidade######### Probito ########

####### Modelo completo ####
shapiro.test(modelo_betat_probit$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_probit) #p-value = 0.04034
#Independência
plot(modelo_betat_probit$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_probit$fitted.values, modelo_betat_probit$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_probit) #p-value = 0.0007128, heterocedasticidade

####### Modelo 5% ####
plot(modelo_betat_probit1, which = 1)
plot(modelo_betat_probit1, which = 2)
plot(modelo_betat_probit1, which = 3)
plot(modelo_betat_probit1, which = 4)
plot(modelo_betat_probit1, which = 5)
plot(modelo_betat_probit1, which = 6)
shapiro.test(modelo_betat_probit1$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_probit1) #p-value = 0.04034
#Independência
plot(modelo_betat_probit1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_probit1$fitted.values, modelo_betat_probit1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_probit1) #p-value = 0.0007128, heterocedasticidade

####### Modelo 10% ####
plot(modelo_betat_probit2, which = 1)
plot(modelo_betat_probit2, which = 2)
plot(modelo_betat_probit2, which = 3)
plot(modelo_betat_probit2, which = 4)
plot(modelo_betat_probit2, which = 5)
plot(modelo_betat_probit2, which = 6)
shapiro.test(modelo_betat_probit2$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_probit2) #p-value = 0.04034
#Independência
plot(modelo_betat_probit2$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_probit2$fitted.values, modelo_betat_probit2$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_probit2) #p-value = 0.0007128, heterocedasticidade

######### cloglog ######
####### Modelo completo ####
shapiro.test(modelo_betat_cloglog$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_cloglog) #p-value = 0.04034
#Independência
plot(modelo_betat_cloglog$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_cloglog$fitted.values, modelo_betat_cloglog$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_cloglog) #p-value = 0.0007128, heterocedasticidade

####### Modelo 5% ####
plot(modelo_betat_cloglog1, which = 1)
plot(modelo_betat_cloglog1, which = 2)
plot(modelo_betat_cloglog1, which = 3)
plot(modelo_betat_cloglog1, which = 4)
plot(modelo_betat_cloglog1, which = 5)
plot(modelo_betat_cloglog1, which = 6)
shapiro.test(modelo_betat_cloglog1$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_cloglog1) #p-value = 0.04034
#Independência
plot(modelo_betat_cloglog1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_cloglog1$fitted.values, modelo_betat_cloglog1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_cloglog1) #p-value = 

####### Modelo 10% ####
plot(modelo_betat_cloglog2, which = 1)
plot(modelo_betat_cloglog2, which = 2)
plot(modelo_betat_cloglog2, which = 3)
plot(modelo_betat_cloglog2, which = 4)
plot(modelo_betat_cloglog2, which = 5)
plot(modelo_betat_cloglog2, which = 6)
shapiro.test(modelo_betat_cloglog2$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_cloglog2) #p-value = 0.04034
#Independência
plot(modelo_betat_cloglog2$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_cloglog2$fitted.values, modelo_betat_cloglog2$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_cloglog2) #p-value = 0.0007128, heterocedasticidade

######### cauchito ######
#### Modelo Completo ####
shapiro.test(modelo_betat_cauchit$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_cauchit) #p-value = 0.04034
#Independência
plot(modelo_betat_cauchit$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_cauchit$fitted.values, modelo_betat_cauchit$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_cauchit) #p-value = 0.0007128, heterocedasticidade

#### Modelo 5% ####
plot(modelo_betat_cauchit1, which = 1)
plot(modelo_betat_cauchit1, which = 2)
plot(modelo_betat_cauchit1, which = 3)
plot(modelo_betat_cauchit1, which = 4)
plot(modelo_betat_cauchit1, which = 5)
plot(modelo_betat_cauchit1, which = 6)
shapiro.test(modelo_betat_cauchit1$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_cauchit1) #p-value = 0.04034
#Independência
plot(modelo_betat_cauchit1$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_cauchit1$fitted.values, modelo_betat_cauchit1$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_cauchit1) #p-value = 0.0007128, heterocedasticidade

#### Modelo 10% ####
plot(modelo_betat_cauchit2, which = 1)
plot(modelo_betat_cauchit2, which = 2)
plot(modelo_betat_cauchit2, which = 3)
plot(modelo_betat_cauchit2, which = 4)
plot(modelo_betat_cauchit2, which = 5)
plot(modelo_betat_cauchit2, which = 6)
shapiro.test(modelo_betat_cauchit2$residuals) #p-value = 
#Teste de durbin watson para independencia
library(lmtest)
dwtest(modelo_betat_cauchit2) #p-value = 0.04034
#Independência
plot(modelo_betat_cauchit2$residuals,
     ylab = "Residuos",
     xlab = "Index dos Imovéis", 
     main = "Suposição de independência",
     pch = 19)

#Homocedasticidade
plot(modelo_betat_cauchit2$fitted.values, modelo_betat_cauchit2$residuals, 
     xlab = "Valores Ajustados",
     ylab = "Residuos",
     pch = 19,
     main = "Suposição de homocedasticidade"
)
#Breusch_Pagan para homocedasticdade
bptest(modelo_betat_cauchit2) #p-value = 0.0007128, heterocedasticidade

#### Não funcionou e não vai funcionar ####
# completo_regp = betareg(WINP_transformado ~ . ,data = playoffs_transformado)
# vazio_regp = betareg(WINP_transformado ~ 1 ,data = playoffs_transformado)
# step(completo_regp, scope=list(upper=completo_regp, lower=vazio_regp), direction='backward', trace=TRUE)