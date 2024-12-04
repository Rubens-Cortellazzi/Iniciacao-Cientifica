source("dados_playoffs.R")

##################### Análise descritiva Playoffs ##############################

#Análise de minimo, maximo, media, mediana, 1 e 3 quartil de todas as variáveis.
summary(dados_playoffs)

############# Análise descritiva da variável resposta ##############

#Medidas básicas
summary((dados_playoffs$WINP))

#Histograma da Porcentagem de Vitória
hist_play <- dados_playoffs %>% ggplot(aes(x = WINP)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(x = "Porcentagem de Vitória",
    y = "Frequência"
  ) +
  theme_bw()
hist_play

#Gráfico de pontos com a Porcentagem de vitórias ao decorrer das temporadas
dados_playoffs %>% ggplot() +
  geom_point(aes(x = Temporada, y = WINP)) +
  theme_bw() +
  labs(title = "Gráfico de pontos dos times",
       y = "Porcentagem de vitórias") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Gráfico de pontos com a Porcentagem de vitórias ao decorrer das temporadas por time
dados_playoffs %>% 
  ggplot() +
  geom_point(aes(x = Temporada, y = WINP, color = TEAM)) +
  theme_bw() +
  labs(title = "Gráfico de pontos dos times",
       y = "Porcentagem de vitórias") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Gráfico de linha com a  Porcentagem de vitórias ao decorrer das temporadas por time 
dados_playoffs %>% ggplot(aes(x = Temporada, y = WINP, color = TEAM, group = TEAM)) +
  geom_line() +
  labs(title = "Variação das Porcentagens de Vitórias por Time",
       x = "Temporada",
       y = "Porcentagem de Vitórias") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Gráfico de linha com a  Porcentagem média de vitórias ao decorrer das temporadas 
dados_playoffs %>% dplyr::select(TEAM,Temporada, WINP) %>% group_by(Temporada) %>% summarise(Media = mean(WINP)) %>% 
  ggplot(aes(x = Temporada, y = Media, group = 1)) +
  geom_line() +
  #scale_y_continuous(limits = c(0.493, 0.505)) +
  labs(title = "Média das Porcentagens de Vitórias por Temporada",
       x = "Temporada",
       y = "Média das Porcentagens de Vitórias") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Boxplot das porcentagens de vitória nas temporadas
dados_playoffs %>% 
  dplyr::select(WINP) %>% 
  ggplot() +
  geom_boxplot(aes(x = WINP)) +
  labs(title = "Boxplot das porcentagens de vitória nas temporadas",
       x = "Porcentagem de vitórias") +
  theme_bw()

#Boxplot da porcentagem de vitórias durante as temporadas
box_play <- dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = WINP, y = Temporada, fill = Temporada), show.legend = FALSE) +
  labs(x = "Porcentagem de vitórias") +
  theme_bw() 
box_play

#Histograma com a porcentagem média de vitórias por temporada
dados_playoffs %>% dplyr::select(TEAM,Temporada, WINP) %>% group_by(Temporada) %>% summarise(Media = mean(WINP)) %>%
  ggplot(aes(x = Temporada, y = Media)) +
  geom_bar(stat = "identity", fill = "blue") +  # stat = "identity" para usar os valores diretamente
  labs(title = "Média das Porcentagens de Vitórias por Temporada",
       x = "Temporada",
       y = "Média das Porcentagens de Vitórias") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

(box_play + hist_play)
######## Análise de 3 pontos #############

#Gráfico de linha com a varição das bolas de 3 tentadas durante as tamporadas
tentadas3 <- dados_playoffs %>% group_by(Temporada) %>% summarise(chute = mean(`3PA`))  %>% 
  ggplot(aes(x = Temporada, y = chute, group = 1)) +
  geom_line() +
  labs(title = "Tentados de 3 pontos",
       x = "Temporada",
       y = "Média de tentativas") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Gráfico de linha com a varição das bolas de 3 acertadas durante as tamporadas
acertadas3 <- dados_playoffs %>% group_by(Temporada) %>% summarise(chute = mean(`3PM`))  %>% 
  ggplot(aes(x = Temporada, y = chute, group = 1)) +
  geom_line() +
  labs(title = "Acertados de 3 pontos",
       x = "Temporada",
       y = "Média de acertos") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Gráfico de linha com a varição da porcentagem de bolas de 3 durante as tamporadas
porcentagem3 <- dados_playoffs %>% group_by(Temporada) %>% summarise(chute = mean(`3PP`))  %>% 
  ggplot(aes(x = Temporada, y = chute, group = 1,xlab="")) +
  geom_line() +
  labs(title = "Porcentagem de 3 pontos",
       x = "Temporada",
       y = "Porcentagem") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
(tentadas3 + acertadas3)/(porcentagem3)
###############################################################################

#################################################################

#Gráfico de linha com a Média dae pontos por temporada
pontos_plyoffs <- dados_playoffs %>% group_by(Temporada) %>% summarise(Media_Pontos = mean(PTS)) %>% 
  ggplot(aes(x = Temporada, y = Media_Pontos, group = 1)) +
  geom_line() +
  labs(title = "Média de pontos nos playoffs",
       x = "Temporada",
       y = "Média das Porcentagens de Vitórias") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

############## Boxplots ao decorrer das temporadas da variável resposta com a variável: ########

#Pontos
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = PTS, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

#Porcentagem de arremessos livres
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = FGP, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

#Porcentagem de bolas de três convertidas
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = `3PP`, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

#Tentativas de lance livre
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = FTA, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

#Rebotes
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = REB, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

#Assistência
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = AST, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

#Turnovers
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = TOV, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

#Roubada de bola
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = STL, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

#Plus_minus
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = PlusMinus, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

#Tocos
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = BLK, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

#Faltas
dados_playoffs %>% 
  ggplot() +
  geom_boxplot(aes(x = PFD, y = Temporada, fill = Temporada), show.legend = FALSE) +
  theme_bw() 

########################### Análise de Correlação #####################
library(GGally)
#Correlação entre todas as variáveis que farão parte do modelo
#A visualização não está muito boa
#dados_playoffs %>% dplyr::select(-c(Posicao,TEAM, GP, W, L, MIN, Temporada)) %>%  ggpairs() #Demora para carregar

#### Análise correlação entre a variável resposta e a seguintes variável:
#Pontos
dados_playoffs %>% dplyr::select(c(WINP, PTS)) %>%  ggpairs()
#Arremessos feitos
dados_playoffs %>% dplyr::select(c(WINP, FGM)) %>%  ggpairs()
#Tentativas de Arremessos 
dados_playoffs %>% dplyr::select(c(WINP, FGA)) %>%  ggpairs()
##Porcentagem de Arremessos 
dados_playoffs %>% dplyr::select(c(WINP, FGP)) %>%  ggpairs() 
#3 pontos feitos
dados_playoffs %>% dplyr::select(c(WINP, `3PM`)) %>%  ggpairs()
#3 pontos tentados
dados_playoffs %>% dplyr::select(c(WINP, `3PA`)) %>%  ggpairs()
#Porcentagem de 3 pontos acertados
dados_playoffs %>% dplyr::select(c(WINP, `3PP`)) %>%  ggpairs()
#Lances livres feitos
dados_playoffs %>% dplyr::select(c(WINP, FTM)) %>%  ggpairs()
#Tentativas de lance livre
dados_playoffs %>% dplyr::select(c(WINP, FTA)) %>%  ggpairs()
##Porcentagem de lance livre
dados_playoffs %>% dplyr::select(c(WINP, FTP)) %>%  ggpairs()
#Rebote ofensivo
dados_playoffs %>% dplyr::select(c(WINP, OREB)) %>%  ggpairs()
#Rebote defensivo
dados_playoffs %>% dplyr::select(c(WINP, DREB)) %>%  ggpairs()
#Rebotes
dados_playoffs %>% dplyr::select(c(WINP, REB)) %>%  ggpairs()
#Assistência
dados_playoffs %>% dplyr::select(c(WINP, AST)) %>%  ggpairs()
#Turnovers
dados_playoffs %>% dplyr::select(c(WINP, TOV)) %>%  ggpairs()
#Rouba de bola
dados_playoffs %>% dplyr::select(c(WINP, STL)) %>%  ggpairs()
#Toco
dados_playoffs %>% dplyr::select(c(WINP, BLK)) %>%  ggpairs()
#Tocos tomados
dados_playoffs %>% dplyr::select(c(WINP, BLKA)) %>%  ggpairs()
#Faltas
dados_playoffs %>% dplyr::select(c(WINP, PF)) %>%  ggpairs()
#Faltas cedidas
dados_playoffs %>% dplyr::select(c(WINP, PFD)) %>%  ggpairs()
#Plus_Minus
dados_playoffs %>% dplyr::select(c(WINP, PlusMinus)) %>%  ggpairs() #Correlação muito alta
cor(dados_playoffs$WINP,dados_playoffs$PlusMinus) #0.860124
#Correlação de caracteristicas defensivas
dados_playoffs %>% dplyr::select(c(WINP, DREB, STL, BLK, PF)) %>%  ggpairs() #Sem correlações de interesse
#Correlação de caracteristicas ofensivas
dados_playoffs %>% dplyr::select(-c(Posicao,TEAM, GP, W, L, MIN, Temporada, DREB, STL, BLK, PF)) %>%  ggpairs()
# FG_P e Plus_Minus correlação de 0,564
# WINP e Plus_Minus Correlação muito alta
# PFD e FTA correlação de 0,866
# PFD e FTM correlação de 0,784
#AST e 3PM correlação 0.542
#AST e PTS correlação 0.683
#AST e FGA correlação 0.539
#AST e FGM correlação 0.732
#REB e OREB correlação 0.540
#REB e FGA correlação de 0.619
# Alta correlação entre FTA e FTM de 0.920
# 3P_P e FG_P correlação 0.5616
#PTS e FGM correlação 0.907
#PTS e FGA correlação 0.656
#FGM e FGA correlação 0.718
#PTS e FG_P correlação 0.589
#FG_P e FGM correlação 0.655
#PTS e 3PM correlação 0.745
#3PM e FGM correlação 0.593
#3PM e FGA correlação 0.514
# 3PA e 3PM correlação de 0.951
#PTS e 3PA correlação 0.683
#3PA e FGM correlação 0.530
#3PA e FGA correlação 0.586

## Aparições em playoffs

aparicoes <- dados_playoffs %>% group_by(TEAM) %>% summarise(Aparicoes = n()) %>% arrange(desc(Aparicoes))
aparicoes

aparicoes_leste <- dados_playoffs %>% filter(Conferencia == "Leste") %>% group_by(TEAM) %>% summarise(Aparicoes = n()) %>% arrange(desc(Aparicoes))
aparicoes_leste

aparicoes_oeste <- dados_playoffs %>% filter(Conferencia == "Oeste") %>% group_by(TEAM) %>% summarise(Aparicoes = n()) %>% arrange(desc(Aparicoes))
aparicoes_oeste
################# Análise das conferências ########################

#Média da porcentagem de vitória por conferência
dados_playoffs %>% group_by(Conferencia) %>% summarise(Media_conf = mean(WINP))

#Teste t para saber se as médias tem diferença significativa
t.test(WINP ~ Conferencia, data = dados_playoffs)

#Summary de cada conferencia de todas as variaveis
dados_playoffs %>% filter(Conferencia == "Leste") %>% summary()

dados_playoffs %>% filter(Conferencia == "Oeste") %>% summary()

#Summary de cada conferencia da variavel resposta
dados_playoffs %>% filter(Conferencia == "Leste") %>% dplyr::select(WINP) %>% summary()
var(dados_playoffs %>% filter(Conferencia == "Leste") %>% dplyr::select(WINP))

dados_playoffs %>% filter(Conferencia == "Oeste") %>% dplyr::select(WINP) %>% summary()
var(dados_playoffs %>% filter(Conferencia == "Oeste") %>% dplyr::select(WINP))

# Boxplot da porcentagem de vitorias por conferencia
dados_playoffs %>% filter(Conferencia == "Leste") %>% dplyr::select(WINP) %>% 
  ggplot() +
  geom_boxplot(aes(x = WINP), fill = "Blue")

dados_playoffs %>% filter(Conferencia == "Oeste") %>% dplyr::select(WINP) %>% 
  ggplot() +
  geom_boxplot(aes(x = WINP), fill = "Red")

# Histograma da porcentagem de vitorias por conferencia
dados_playoffs %>% filter(Conferencia == "Leste") %>% dplyr::select(WINP) %>% 
  ggplot() +
  geom_histogram(aes(x = WINP), fill = "Blue", color = "black")

dados_playoffs %>% filter(Conferencia == "Oeste") %>% dplyr::select(WINP) %>% 
  ggplot() +
  geom_histogram(aes(x = WINP), fill = "Red", color = "black")

######################## Leste ####################

#Gráfico de pontos com a Porcentagem de vitórias ao decorrer das temporadas
dados_playoffs %>% filter(Conferencia == "Leste") %>% ggplot() +
  geom_point(aes(x = Temporada, y = WINP)) +
  theme_bw() +
  labs(title = "Gráfico de pontos dos times do Leste",
       y = "Porcentagem de vitórias") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Gráfico de pontos com a Porcentagem de vitórias ao decorrer das temporadas por time
dados_playoffs %>% filter(Conferencia == "Leste") %>%
  ggplot() +
  geom_point(aes(x = Temporada, y = WINP, color = TEAM)) +
  theme_bw() +
  labs(title = "Gráfico de pontos dos times do Leste",
       y = "Porcentagem de vitórias") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Gráfico de linha com Variação das Porcentagens de Vitórias por Time do Leste
dados_playoffs %>% filter(Conferencia == "Leste") %>% ggplot(aes(x = Temporada, y = WINP, color = TEAM, group = TEAM)) +
  geom_line() +
  labs(title = "Variação das Porcentagens de Vitórias por Time do Leste",
       x = "Temporada",
       y = "Porcentagem de Vitórias") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Gráfico de linha com Média das Porcentagens de Vitórias por Temporada do Leste
dados_playoffs %>% filter(Conferencia == "Leste") %>% dplyr::select(TEAM,Temporada, WINP, Conferencia) %>% group_by(Temporada) %>% summarise(Media = mean(WINP)) %>% 
  ggplot(aes(x = Temporada, y = Media, group = 1)) +
  geom_line() +
  labs(title = "Média das Porcentagens de Vitórias por Temporada do Leste",
       x = "Temporada",
       y = "Média das Porcentagens de Vitórias") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Boxplot das porcentagens de vitória nas temporadas do Leste
dados_playoffs %>% filter(Conferencia == "Leste") %>%
  ggplot() +
  geom_boxplot(aes(x = WINP, y = Temporada, fill = Temporada), show.legend = FALSE) +
  labs(title = "Boxplot das porcentagens de vitória nas temporadas do Leste",
       x = "Porcentagem de vitórias") +
  theme_bw() 

#Gráfico de linha com Média de pontos por Temporada do Leste
dados_playoffs %>% filter(Conferencia == "Leste") %>% group_by(Temporada) %>% summarise(Media_Pontos = mean(PTS)) %>% 
  ggplot(aes(x = Temporada, y = Media_Pontos, group = 1)) +
  geom_line() +
  labs(title = "Média de pontos por Temporada do Leste",
       x = "Temporada",
       y = "Média das Porcentagens de Vitórias") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Quantidade de vezes que o time do Leste ficou em cada posição
dados_playoffs %>% filter(Conferencia == "Leste") %>% count(Posicao)  

###################### Oeste ######################

#Gráfico de pontos com a Porcentagem de vitórias ao decorrer das temporadas
dados_playoffs %>% filter(Conferencia == "Oeste") %>% ggplot() +
  geom_point(aes(x = Temporada, y = WINP)) +
  theme_bw() +
  labs(title = "Gráfico de pontos dos times do Oeste",
       y = "Porcentagem de vitórias") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Gráfico de pontos com a Porcentagem de vitórias ao decorrer das temporadas por time
dados_playoffs %>% filter(Conferencia == "Oeste") %>%
  ggplot() +
  geom_point(aes(x = Temporada, y = WINP, color = TEAM)) +
  theme_bw() +
  labs(title = "Gráfico de pontos dos times do Oeste",
       y = "Porcentagem de vitórias") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus


#Gráfico de linha com Variação das Porcentagens de Vitórias por Time do Oeste
dados_playoffs %>% filter(Conferencia == "Oeste") %>% ggplot(aes(x = Temporada, y = WINP, color = TEAM, group = TEAM)) +
  geom_line() +
  labs(title = "Variação das Porcentagens de Vitórias por Time do Oeste",
       x = "Temporada",
       y = "Porcentagem de Vitórias") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Gráfico de linhas com Média das Porcentagens de Vitórias por Temporada do Oeste
dados_playoffs %>% filter(Conferencia == "Oeste") %>% dplyr::select(TEAM,Temporada, WINP, Conferencia) %>% group_by(Temporada) %>% summarise(Media = mean(WINP)) %>% 
  ggplot(aes(x = Temporada, y = Media, group = 1)) +
  geom_line() +
  labs(title = "Média das Porcentagens de Vitórias por Temporada do Oeste",
       x = "Temporada",
       y = "Média das Porcentagens de Vitórias") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Boxplot das porcentagens de vitória nas temporadas do Oeste
dados_playoffs %>% filter(Conferencia == "Oeste") %>%
  ggplot() +
  geom_boxplot(aes(x = WINP, y = Temporada, fill = Temporada), show.legend = FALSE) +
  labs(title = "Boxplot das porcentagens de vitória nas temporadas do Oeste",
       x = "Porcentagem de vitórias") +
  theme_bw() 

#Gráfico de linha com a Média dae pontos por temporada
dados_playoffs %>% filter(Conferencia == "Oeste") %>% group_by(Temporada) %>% summarise(Media_Pontos = mean(PTS)) %>% 
  ggplot(aes(x = Temporada, y = Media_Pontos, group = 1)) +
  geom_line() +
  labs(title = "Média de pontos por Temporada do Oeste",
       x = "Temporada",
       y = "Média das Porcentagens de Vitórias") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos em 45 graus

#Quantidade de vezes que o time do oeste ficou em cada posição
dados_playoffs %>% filter(Conferencia == "Oeste") %>% count(Posicao)