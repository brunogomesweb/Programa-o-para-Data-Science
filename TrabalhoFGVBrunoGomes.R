#Você deverá realizar análises que respondam os questionamentos abaixo:
  
#1)	Apresente as frequências e as porcentagens das variáveis qualitativas.
#2)	Apresente a média, o mínimo e o máximo das variáveis quantitativas.
#3)	Entre os clientes adimplentes, apresente o número de clientes, a idade máxima, a % média da renda do cliente comprometida com o empréstimo para cada nível de escolaridade.
#4)	Apresente um boxplot para a % da renda do cliente comprometida com o empréstimo por sexo.
#5)	Apresente uma tabela de contingência para o cruzamento das variáveis situação e escolaridade. Faça um gráfico apropriado para acompanhar a tabela.
#6)	Faça um gráfico que avalia a relação entre as variáveis valor do empréstimo e % da renda comprometida com o empréstimo para clientes que tem mais de 2 anos na empresa.
#7)	Apresente o gráfico anterior para cada nível de escolaridade.

library(tidyverse)
library(readxl)
library(stringr)
library(data.table)
library(expss)
library(tidyr)

#Atribuindo 999 para os NA pois pois já existe esse padrão no DS
BaseCredito <- read_excel(path = "Base_Credito.xlsx", na = "999") 
BaseCredito

#Tratando as variaveis 
BaseCredito$Situacao = factor(x = BaseCredito$Situacao,
                         labels = c("Adimplente","Inadimplente"))

BaseCredito$Sexo = factor(x = BaseCredito$Sexo,
                          labels = c("Masculino","Feminino"))

BaseCredito$Escolaridade = factor(x = BaseCredito$Escolaridade,
                          levels = c(1,2,3,4,5,6,7),
                          labels = c("Fundamental Incompleto","Fundamental Completo"
                                     ,"Medio Incompleto","Medio Completo","Superior","Mestrado","Doutorado"))

BaseCredito$Estado_civil = factor(x = BaseCredito$Estado_civil,
                          levels = c(1,2,3,4),
                          labels = c("Solteiro","Casado","Viuvo","Divorciado"))

BaseCredito$hist_inadimplencia = factor(x = BaseCredito$hist_inadimplencia,
                                  labels = c("Não","Sim"))

BaseCredito$atrasou_parcela = factor(x = BaseCredito$atrasou_parcela,
                                        labels = c("Não","Sim"))

BaseCredito

#1)	Apresente as frequências e as porcentagens das variáveis qualitativas.
BaseCredito %>% 
  select(Situacao) %>% 
  fre

BaseCredito %>% 
  select(Sexo) %>% 
  fre

BaseCredito %>% 
  select(Escolaridade) %>% 
  fre

BaseCredito %>% 
  select(Estado_civil) %>% 
  fre

BaseCredito %>% 
  select(hist_inadimplencia) %>% 
  fre

BaseCredito %>% 
  select(atrasou_parcela) %>% 
  fre

#2)	Apresente a média, o mínimo e o máximo das variáveis quantitativas.
BaseCredito
BaseCredito %>%
  summarise(media = mean(Emprestimo, na.rm = TRUE),
            minimo = min(Emprestimo, na.rm = TRUE),
            maximo = max(Emprestimo, na.rm = TRUE),
            mediana = median(Emprestimo, na.rm = TRUE),
            quartil1 = quantile(Emprestimo, probs = .25, na.rm = TRUE),
            variancia = var(Emprestimo, na.rm = TRUE),
            desvio = sd(Emprestimo, na.rm = TRUE),
            dam = mad(Emprestimo, na.rm = TRUE),
            CV = desvio/media)

#3)	Entre os clientes adimplentes, apresente o número de clientes, a idade máxima, a % média da renda do 
#cliente comprometida com o empréstimo para cada nível de escolaridade.
clientes_adimplentes = BaseCredito %>%
  group_by(Escolaridade) %>% 
  filter(Situacao == "Adimplente") %>% 
  summarise(qtdclientes = n(),
            IdadeMaxima = max(Idade, na.rm = TRUE),
            MediaRenda = mean(por_renda, na.rm = TRUE))
clientes_adimplentes

#4)	Apresente um boxplot para a % da renda do cliente comprometida com o empréstimo por sexo.
#box1 = BaseCredito %>%
#  filter(!is.na(por_renda)) %>% 
#  summarise(Sexo = Sexo, rendacomprometida = mean(por_renda)) %>% 
#  ggplot(aes(x = Sexo, y = rendacomprometida)) +  
#  geom_boxplot()
#box1

box1 = BaseCredito %>%
  filter(!is.na(por_renda)) %>% 
  ggplot(aes(x = Sexo, y = por_renda)) +  
  geom_boxplot() +
  labs(x = "Sexo", y = "% Renda comprometida")
box1

#5)	Apresente uma tabela de contingência para o cruzamento das variáveis situação e escolaridade. 
#Faça um gráfico apropriado para acompanhar a tabela.

BaseCredito %>%
  filter(!is.na(Situacao) & !is.na(Escolaridade)) %>% 
  select(Situacao, Escolaridade) %>% 
  calc_cro(Situacao, Escolaridade) %>%
  ggplot(aes(Situacao)) + 
  geom_bar(aes(fill = Escolaridade))

grafb1 = BaseCredito %>% 
  filter(!is.na(Situacao) & !is.na(Escolaridade)) %>% 
  ggplot(aes(x = Escolaridade)) + 
  geom_bar(aes(fill = Situacao))
grafb1

grafb2 = BaseCredito %>% 
  filter(!is.na(Situacao) & !is.na(Escolaridade)) %>% 
  ggplot(aes(x = Escolaridade)) + 
  geom_bar(aes(fill = Situacao), position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
grafb2


