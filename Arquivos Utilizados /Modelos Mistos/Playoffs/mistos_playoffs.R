source("dados_playoffs.R")

library(gamlss)
#Referencia: https://rdrr.io/cran/gamlss/man/random.html

########### Modelos Mistos GAMLSS ##########
#### Normal ####
##### Modelo Completo TEAM ####
misto_normal_completop <- gamlss(WINP ~ . + re(random=~1|TEAM), data = dados_regressaop, family = NO)
summary(misto_normal_completop)
coef(misto_normal_completop)
getSmo(misto_normal_completop)

######Modelo 10% #####
misto_normal1p <- gamlss(WINP ~ PTS + FGM + FGA + FGP + `3PM` + FTM + OREB + DREB + REB+ PF + PlusMinus + re(random=~1|TEAM), data = dados_regressaop, family = NO)
summary(misto_normal1p)
coef(misto_normal1p)
getSmo(misto_normal1p)

##### Backward ##### 
misto_normal_completop 
misto_normal_vaziop <- gamlss(WINP ~ 1 + re(random=~1|TEAM), data = dados_regressaop, family = NO)
step(misto_normal_completop, scope=list(upper=misto_normal_completop, lower=misto_normal_vaziop), direction='backward', trace=TRUE)

misto_normal_backp <- gamlss(formula = WINP ~ PTS + FGM + FGP + `3PM` + FTM + FTP + OREB +  
                               DREB + REB + STL + PF + PFD + PlusMinus + (re(random = ~1 |TEAM)),
                             family = NO, data = dados_regressaop)

##### Forward ####
step(misto_normal_vaziop, scope=list(upper=misto_normal_completop, lower=misto_normal_vaziop), direction='forward', trace=TRUE)

misto_normal_forwp <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) +  
                               PlusMinus + DREB, family = NO, data = dados_regressaop) 

##### Anova #####
misto_normal1p #PTS + FGM + FGA + FGP + `3PM` + FTM + OREB + DREB + REB + PF + PlusMinus + re(random = ~1 | TEAM)
misto_normal_backp# PTS + FGM + FGP + `3PM` + FTM +  FTP + OREB + DREB + REB + STL + PF + PFD + PlusMinus + (re(random = ~1 | TEAM))
misto_normal_forwp #(re(random = ~1 | TEAM)) + PlusMinus + DREB
misto_normal0p <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)), family = NO, data = dados_regressaop) 
sem_misto_normal0p <- gamlss(formula = WINP ~ 1, family = NO, data = dados_regressaop) 
misto_normal_plusp <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus, family = NO, data = dados_regressaop) 
misto_normalp_pf <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + PF, family = NO, data = dados_regressaop) 
misto_normalp_reb <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + REB, family = NO, data = dados_regressaop) 
misto_normalp_dreb <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + DREB, family = NO, data = dados_regressaop) 
misto_normalp_oreb <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + DREB + OREB, family = NO, data = dados_regressaop) 
misto_normalp_ftm <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + DREB + FTM, family = NO, data = dados_regressaop) 
misto_normalp_3pm <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + DREB + `3PM`, family = NO, data = dados_regressaop) 
misto_normalp_fgm <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + DREB + FGM, family = NO, data = dados_regressaop) 
misto_normalp_pts <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + DREB + PTS, family = NO, data = dados_regressaop) 
misto_normalp_fgp <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + DREB + FGP, family = NO, data = dados_regressaop) 
misto_normalp_fga <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + DREB + FGA, family = NO, data = dados_regressaop) 

lrtest(sem_misto_normal0p, misto_normal0p) #2.436e-06, efeito aleatório significante
lrtest(misto_normal0p, misto_normal_plusp) #2.2e-16, plus minus significante
lrtest(misto_normal_plusp, misto_normalp_pf) #0.1073 PF não significante
lrtest(misto_normal_plusp, misto_normalp_reb) #0.2284, REB não significante
lrtest(misto_normal_plusp, misto_normalp_dreb) #0.06485, DREB significante
lrtest(misto_normalp_dreb, misto_normalp_oreb) #0.6006, OREB não significante
lrtest(misto_normalp_dreb, misto_normalp_ftm) #0.9601, FTM não significante
lrtest(misto_normalp_dreb, misto_normalp_3pm) #0.5557, 3PM não significante
lrtest(misto_normalp_dreb, misto_normalp_fgm) #0.2131, FGM não significante
lrtest(misto_normalp_dreb, misto_normalp_pts) # 0.3123 PTS não significante
lrtest(misto_normalp_dreb, misto_normalp_fgp) #0.6707, FGP não significante
lrtest(misto_normalp_dreb, misto_normalp_fga) #0.3517, FGA não significante

#Melhor modelo é misto_normalp_dreb com Plus Minus, DREB e (re(random = ~1 | TEAM)) 

##### Modelo Completo Temporada ####
misto_normalp_completo_temp <- gamlss(WINP ~ . + re(random=~1|Numero_temporada), data = dados_regressaop, family = NO)
summary(misto_normalp_completo_temp)
coef(misto_normalp_completo_temp)
getSmo(misto_normalp_completo_temp)
##### Backward Temp##### 
misto_normalp_completo_temp 
misto_normalp_vazio_temp <- gamlss(WINP ~ 1 + re(random=~1|Numero_temporada), data = dados_regressaop, family = NO)
step(misto_normalp_completo_temp, scope=list(upper=misto_normalp_completo_temp, lower=misto_normalp_vazio_temp), direction='backward', trace=TRUE)

misto_normalp_back_temp <- gamlss(formula = WINP ~ TEAM + PTS + FGM + FGA + FGP +  
                                   `3PM` + FTM + OREB + DREB + REB + BLKA + PF + PlusMinus +  
                                   (re(random = ~1 | Numero_temporada)), family = NO,  
                                 data = dados_regressaop) 

##### Forward Temp####
step(misto_normalp_vazio_temp, scope=list(upper=misto_normalp_completo_temp, lower=misto_normalp_vazio_temp), direction='forward', trace=TRUE)

misto_normalp_forw_temp <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) +  
                                    PlusMinus + DREB + TEAM + BLKA, family = NO, data = dados_regressaop) 

##### Anova ####
misto_normalp_back_temp #TEAM + PTS + FGM + FGA + FGP + `3PM` + FTM + OREB + DREB + REB + BLKA + PF + PlusMinus
misto_normalp_forw_temp # PlusMinus + DREB + TEAM + BLKA
misto_normalp_temp0 <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)), family = NO, data = dados_regressaop) 
sem_misto_normalp_temp0 <- gamlss(formula = WINP ~ 1, family = NO, data = dados_regressaop) 
misto_normalp_temp_plus <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus, family = NO, data = dados_regressaop) 
misto_normalp_temp_dreb <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB, family = NO, data = dados_regressaop) 
misto_normalp_temp_blka <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + BLKA, family = NO, data = dados_regressaop) 
misto_normalp_temp_team <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM, family = NO, data = dados_regressaop) 
misto_normalp_temp_pts <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM + PTS, family = NO, data = dados_regressaop) 
misto_normalp_temp_fgm <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM + FGM, family = NO, data = dados_regressaop) 
misto_normalp_temp_fga <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM + FGA, family = NO, data = dados_regressaop) 
misto_normalp_temp_fgp <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM + FGP, family = NO, data = dados_regressaop) 
misto_normalp_temp_pf <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM + PF, family = NO, data = dados_regressaop) 
misto_normalp_temp_reb <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM + REB, family = NO, data = dados_regressaop) 
misto_normalp_temp_oreb <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM + OREB, family = NO, data = dados_regressaop) 
misto_normalp_temp_ftm <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM + FTM, family = NO, data = dados_regressaop) 
misto_normalp_temp_3pm <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM + `3PM`, family = NO, data = dados_regressaop) 

lrtest(sem_misto_normalp_temp0, misto_normalp_temp0) #Temporada não foi significativo
lrtest(misto_normalp_temp0, misto_normalp_temp_plus) #2.2e-16, plus minus foi significativo
lrtest(misto_normalp_temp_plus, misto_normalp_temp_dreb) # 0.0956, dreb foi significativo
lrtest(misto_normalp_temp_dreb, misto_normalp_temp_blka) #0.3559, blka não foi significativo
lrtest(misto_normalp_temp_dreb, misto_normalp_temp_team) # 0.0003774, TEAM foi significativo
lrtest(misto_normalp_temp_team, misto_normalp_temp_pts) # 0.3512, pts não foi significativo
lrtest(misto_normalp_temp_team, misto_normalp_temp_fgm) # 0.3251, fgm não foi significativo
lrtest(misto_normalp_temp_team, misto_normalp_temp_fga) # 0.2123, fga não foi significativo
lrtest(misto_normalp_temp_team, misto_normalp_temp_fgp) # 0.9247, fgp não foi significativo
lrtest(misto_normalp_temp_team, misto_normalp_temp_pf) # 0.1916, pf não foi significativo
lrtest(misto_normalp_temp_team, misto_normalp_temp_reb) #  0.8099, reb não foi significativo
lrtest(misto_normalp_temp_team, misto_normalp_temp_oreb) # 0.7703, oreb não foi significativo
lrtest(misto_normalp_temp_team, misto_normalp_temp_ftm) # 0.8199, ftm não foi significativo
lrtest(misto_normalp_temp_team, misto_normalp_temp_3pm) # 0.6561, 3pm não foi significativo

#Não foi significante temp mas vamos verificar o modelo como um todo
#Então o melhor modelo é misto_normalp_temp_team com (re(random = ~1 | Numero_temporada)) + PlusMinus + DREB + TEAM

#### Beta ####
##### Modelo Completo TEAM ####
misto_betap_completo <- gamlss(WINP ~ . + re(random=~1|TEAM), data = dados_regressaop, family = BEZI)
summary(misto_betap_completo)
coef(misto_betap_completo)
getSmo(misto_betap_completo)

##### Backward TEAM ##### 
misto_betap_completo 
misto_beta_vaziop <- gamlss(WINP ~ 1 + re(random=~1|TEAM), data = dados_regressaop, family = BEZI)
step(misto_betap_completo, scope=list(upper=misto_betap_completo, lower=misto_beta_vaziop), direction='backward', trace=TRUE)

misto_beta_backp <- gamlss(formula = WINP ~ FGM + FGA + FGP + FTP + REB +  
                            STL + PF + PFD + PlusMinus + (re(random = ~1 |  
                                                               TEAM)), family = BEZI, data = dados_regressaop)

##### Forward TEAM ####
step(misto_beta_vaziop, scope=list(upper=misto_betap_completo, lower=misto_beta_vaziop), direction='forward', trace=TRUE)

misto_beta_forwp <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) +  
                             PlusMinus + PF + BLKA, family = BEZI, data = dados_regressaop) 

##### Anova #####
misto_beta_backp #FGM + FGA + FGP + FTP + REB + STL + PF + PFD + PlusMinus 
misto_beta_forwp #PlusMinus + PF + BLKA
misto_betap0 <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)), family = BEZI, data = dados_regressaop) 
sem_misto_betap0 <- gamlss(formula = WINP ~ 1, family = BEZI, data = dados_regressaop) 
misto_betap_plus <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus, family = BEZI, data = dados_regressaop) 
misto_betap_pf <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + PF, family = BEZI, data = dados_regressaop) 
misto_betap_blka <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + PF + BLKA, family = BEZI, data = dados_regressaop) 
misto_betap_pfd <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + PF + BLKA +  PFD, family = BEZI, data = dados_regressaop) 
misto_betap_stl <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + PF + BLKA + STL, family = BEZI, data = dados_regressaop) 
misto_betap_reb <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + PF + BLKA +  REB, family = BEZI, data = dados_regressaop) 
misto_betap_ftp <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + PF + BLKA +  FTP, family = BEZI, data = dados_regressaop) 
misto_betap_fgp <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + PF + BLKA +  FTP + FGP, family = BEZI, data = dados_regressaop) 
misto_betap_fga <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + PF + BLKA +  FTP + FGA, family = BEZI, data = dados_regressaop) 
misto_betap_fgm <- gamlss(formula = WINP ~ (re(random = ~1 | TEAM)) + PlusMinus + PF + BLKA +  FTP + FGM, family = BEZI, data = dados_regressaop) 

lrtest(sem_misto_betap0, misto_betap0) #efeito aleatório significante
lrtest(misto_betap0, misto_betap_plus) # 2.2e-16, plus minus significante
lrtest(misto_betap_plus, misto_betap_pf) # 0.01546, pf significante
lrtest(misto_betap_pf, misto_betap_blka) # 0.06696, blka significante
lrtest(misto_betap_blka, misto_betap_pfd) # 0.2005, pfd não significante
lrtest(misto_betap_blka, misto_betap_stl) # 0.2895, stl não significante
lrtest(misto_betap_blka, misto_betap_reb) # 0.8837, reb não significante
lrtest(misto_betap_blka, misto_betap_ftp) # 2.2e-16, FTP significante
lrtest(misto_betap_ftp, misto_betap_fgp) # 0.5984, FGP não significante
lrtest(misto_betap_ftp, misto_betap_fga) # 0.4514, FGA não significante
lrtest(misto_betap_ftp, misto_betap_fgm) #  0.4515, FGM não significante

#Melhor modelomisto_betap_ftp com PlusMinus + PF + BLKA + FTP

##### Modelo Completo Temporada ####
misto_betap_completo_temp <- gamlss(WINP ~ . + re(random=~1|Numero_temporada), data = dados_regressaop, family = BEZI)
summary(misto_betap_completo_temp)
coef(misto_betap_completo_temp)
getSmo(misto_betap_completo_temp)
##### Backward Temp ##### 
misto_betap_completo_temp 
misto_betap_vazio_temp <- gamlss(WINP ~ 1 + re(random=~1|Numero_temporada), data = dados_regressaop, family = BEZI)
step(misto_betap_completo_temp, scope=list(upper=misto_betap_completo_temp, lower=misto_betap_vazio_temp), direction='backward', trace=TRUE)

misto_betap_back_temp <- gamlss(formula = WINP ~ FGM + FGA + FGP + FTP + PF +  
                                  PlusMinus + (re(random = ~1 | Numero_temporada)),  
                                family = BEZI, data = dados_regressaop)   

##### Forward Temp ####
step(misto_betap_vazio_temp, scope=list(upper=misto_betap_completo_temp, lower=misto_betap_vazio_temp), direction='forward', trace=TRUE)

misto_betap_forw_temp <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) +  
                                  PlusMinus + PF + BLKA, family = BEZI, data = dados_regressaop)

##### Anova ####
misto_betap_back_temp # FGM + FGA + FGP + FTP + PF +  PlusMinus
misto_betap_forw_temp #PlusMinus + PF + BLKA
misto_betap_temp0 <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)), family = BEZI, data = dados_regressaop) 
sem_misto_betap_temp0 <- gamlss(formula = WINP ~ 1, family = BEZI, data = dados_regressaop) 
misto_betap_temp_plus <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus, family = BEZI, data = dados_regressaop) 
misto_betap_temp_pf <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + PF, family = BEZI, data = dados_regressaop) 
misto_betap_temp_blka <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + PF + BLKA, family = BEZI, data = dados_regressaop) 
misto_betap_temp_ftp <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + PF + FTP, family = BEZI, data = dados_regressaop) 
misto_betap_temp_fgp <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + PF + FGP, family = BEZI, data = dados_regressaop) 
misto_betap_temp_fga <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + PF + FGA, family = BEZI, data = dados_regressaop) 
misto_betap_temp_fgm <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + PF + FGM, family = BEZI, data = dados_regressaop) 
misto_betap_temp_team <- gamlss(formula = WINP ~ (re(random = ~1 | Numero_temporada)) + PlusMinus + PF + TEAM, family = BEZI, data = dados_regressaop) 

lrtest(sem_misto_betap_temp0, misto_betap_temp0)#0.9998 Numero temporada não significante
lrtest(misto_betap_temp0, misto_betap_temp_plus)#2.2e-16 plus minus significante
lrtest(misto_betap_temp_plus, misto_betap_temp_pf)#0.01893 pf significante
lrtest(misto_betap_temp_pf, misto_betap_temp_blka)#0.1333 blka não significante
lrtest(misto_betap_temp_pf, misto_betap_temp_ftp)#0.1863 FTP não significante
lrtest(misto_betap_temp_pf, misto_betap_temp_fgp)#0.3049 FGP não significante
lrtest(misto_betap_temp_pf, misto_betap_temp_fga)#0.4589 FGA não significante
lrtest(misto_betap_temp_pf, misto_betap_temp_fgm)#0.9587 FGM não significante
lrtest(misto_betap_temp_pf, misto_betap_temp_team)#TEAM significante

#Melhor modelo misto_betap_temp_pf foi o melhor modelo PlusMinus + PF +TEAM

########### Outras formas de fazer o gamlss ####

#Essas duas funções dão certo e apresentam os mesmos resultados
analise4 <- gamlss::gamlss(WINP ~ . + random(TEAM), data = dados_regressaop, family = BE, method = mixed())
analise5 <- gamlss::gamlss(WINP ~ . + random(TEAM), data = dados_regressaop, family = BE)

# use re() with fixed effect within re() ## DEMORA MAIS PARA RODAR
t2<-gamlss(WINP ~ re(fixed=~., random=~1|TEAM), data = dados_regressaop, family = BE)

#Essa próxima função tem os mesmos resultados que a analise4 e analise5
# use re() with fixed effect in gamlss formula
t3 <- gamlss(WINP ~ . + re(random=~1|TEAM), data = dados_regressaop, family = BE)
######### Utilizando GLM, pois normal foi melhor do que beta #####

#Melhor modelo do gamlss para normal:
#(re(random = ~1 | TEAM)) + PlusMinus + OREB + PF + `3PA`

#Pacote que será utilizado
library(lme4)

#Modelos encontrados no gamlss para tomar como base para o glm
misto_normal1p #PTS + FGM + FGA + FGP + `3PM` + FTM + OREB + DREB + REB + PF + PlusMinus + re(random = ~1 | TEAM)
misto_normal_backp# PTS + FGM + FGP + `3PM` + FTM +  FTP + OREB + DREB + REB + STL + PF + PFD + PlusMinus + (re(random = ~1 | TEAM))
misto_normal_forwp #(re(random = ~1 | TEAM)) + PlusMinus + DREB
## Anova ##
glmp_completo <- lmer(WINP ~ . + (1|TEAM), data = dados_regressaop)
glmp_vazio <- lmer(WINP ~ (1|TEAM), data = dados_regressaop)
glmp_nada <- lm(WINP ~ 1, data = dados_regressaop)
glmp_plus <- lmer(WINP ~ PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_pf <- lmer(WINP ~ PF + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_oreb <- lmer(WINP ~ OREB + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_3pa <- lmer(WINP ~ `3PA` + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_fgp <- lmer(WINP ~ FGP + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_pfd <- lmer(WINP ~ PFD + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_stl <- lmer(WINP ~ STL + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_tov <- lmer(WINP ~ TOV + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_blk <- lmer(WINP ~ BLK + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_blka <- lmer(WINP ~ BLKA + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_ast <- lmer(WINP ~ AST + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_reb <- lmer(WINP ~ REB + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_dreb <- lmer(WINP ~ DREB + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_ftp <- lmer(WINP ~ FTP + DREB + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_fta <- lmer(WINP ~ FTM + DREB + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_ftm <- lmer(WINP ~ FTP + DREB + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_3pp <- lmer(WINP ~ `3PP` + DREB + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_3pm <- lmer(WINP ~ `3PM` + DREB + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_fga <- lmer(WINP ~ FGA + DREB + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_fgm <- lmer(WINP ~ FGM + DREB + PlusMinus + (1|TEAM), data = dados_regressaop)
glmp_pts <- lmer(WINP ~ PTS + DREB + PlusMinus + (1|TEAM), data = dados_regressaop)

anova(glmp_vazio, glmp_plus) #PlusMinus significante
anova(glmp_plus, glmp_pf) #Pf não significante
anova(glmp_plus, glmp_oreb) #OREB não significante
anova(glmp_plus, glmp_3pa) #3pa não significante
anova(glmp_plus, glmp_fgp) #FGP não significante
anova(glmp_plus, glmp_pfd) #Pfd não significante
anova(glmp_plus, glmp_stl) #stl não significante
anova(glmp_plus, glmp_tov) #tov não significante
anova(glmp_plus, glmp_blk) #blk não significante
anova(glmp_plus, glmp_blka) #blka não significante
anova(glmp_plus, glmp_ast) #ast não significante
anova(glmp_plus, glmp_reb) #reb não significante
anova(glmp_plus, glmp_dreb) #dreb não significante
anova(glmp_dreb, glmp_ftp) #ftp não significante
anova(glmp_dreb, glmp_fta) #fta não significante
anova(glmp_dreb, glmp_ftm) #ftm não significante
anova(glmp_dreb, glmp_3pp) #3pp não significante
anova(glmp_dreb, glmp_3pm) #3pm não significante
anova(glmp_dreb, glmp_fga) #fga não significante
anova(glmp_dreb, glmp_fgm) #fgm não significante
anova(glmp_dreb, glmp_pts) #pts não significante

#Melhor modelo é glmp_dreb com DREB + PlusMinus + (1 | TEAM)
#Assim, notamos que é o mesmo modelo do que o gamlss normal.
#Assim, iremos utilizar o glm ao invés do gamlss.

#Pegamos o mesmo modelo escolhido pelo gamlss
glmp_melhor <- lmer(WINP ~ DREB + PlusMinus + (1 | TEAM), data = dados_regressaop)
summary(glmp_melhor)
anova(glmp_melhor)
coef(glmp_melhor)

#Assim, como notamos que para team o gamlss e lmer foram os mesmos modelos,
#vamos adotar que para numero de temporada também é a mesma.
#Assim,(1 | Numero_temporada) + PlusMinus + FGP + PF + FGM + TEAM
#é o melhor modelo.
glm_melhor_temp <- lmer(WINP ~ PlusMinus + FGP + PF + FGM + TEAM + (1|Numero_temporada), data = dados_regressaop)