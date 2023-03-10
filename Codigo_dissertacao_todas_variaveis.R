# C?digo de an?lise para disserta??o de mestrado
# Estudante: Nat? Gomes de Lima Stavinski

#Carregamento de bibliotecas
library(readxl)
library(dplyr)
library(car)
library(rstatix)
library(lmtest)
library(ggpubr)
library(QuantPsyc)
library(psych)
library(scatterplot3d)
library(ggplot2)
library(ggpubr)

#Estabelecendo meu diret?rio
setwd("C:/Users/Usuario/OneDrive/Mestrado UEL/Disserta??o e projeto/Arquivos para defesa/Bancos de dados e c?digos/Banco de dados organizado e limpo")

#Carregando banco de dados
dados<- readxl::read_xlsx("Dados_disserta??o.xlsx", sheet = 1)
dados$HIPERTENSAO<- factor(dados$HIPERTENSAO, levels = c(0:1),
                           labels = c("N?O", "SIM"))



#An?lises principais
#FOR?A ISOINERCIAL

#SUPINO
supino<- lm(DELTA_RM_SUPINO ~ DELTA_SMM + RM_SUPINO + SMM, dados)
AIC(supino)
par(mfrow=c(2,2))
plot(supino)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(supino$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(supino))      #Observando a presen?a de outliers
car::durbinWatsonTest(supino)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(supino)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(supino)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(supino)                 #Resumo do modelo do supino
confint(supino)                 #IC95%
par(mfrow=c(1,1))



#EXTENSORA
extensora<- lm(DELTA_RM_EXTENSORA ~ DELTA_SMM + SMM + RM_EXTENSORA, dados)
AIC(extensora)
par(mfrow=c(2,2))
plot(extensora)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(extensora$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(extensora))      #Observando a presen?a de outliers
car::durbinWatsonTest(extensora)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(extensora)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(extensora)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(extensora)                 #Resumo do modelo do supino
confint(extensora)                 #IC95%
par(mfrow=c(1,1))


#ROSCA
rosca<- lm(DELTA_RM_ROSCA ~ DELTA_SMM + SMM + RM_ROSCA, dados)
AIC(rosca)
par(mfrow=c(2,2))
plot(rosca)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(rosca$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(rosca))      #Observando a presen?a de outliers
car::durbinWatsonTest(rosca)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(rosca)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(rosca)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(rosca)                 #Resumo do modelo do supino
confint(rosca)                 #IC95%
par(mfrow=c(1,1))

#SOMA DOS RM
somarm<- lm(DELTA_SOMA_RM ~ DELTA_SMM + SMM + SOMA_RM, dados)
par(mfrow=c(2,2))
plot(somarm)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(somarm$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(somarm))      #Observando a presen?a de outliers
car::durbinWatsonTest(somarm)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(somarm)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(somarm)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(somarm)                 #Resumo do modelo do supino
confint(somarm)                 #IC95%
par(mfrow=c(1,1))


#FOR?A ISOCIN?TICA
#EXTENS?O ISOCIN?TICA A 60? POR SEGUNDO
isokext60<- lm(DELTA_PTEXT60 ~ DELTA_SMM, dados)
AIC(isokext60)
par(mfrow=c(2,2))
plot(isokext60)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(isokext60$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(isokext60))      #Observando a presen?a de outliers
car::durbinWatsonTest(isokext60)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(isokext60)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(isokext60)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(isokext60)                 #Resumo do modelo do supino
confint(isokext60)                 #IC95%
par(mfrow=c(1,1))

#FLEX?O ISOCIN?TICA A 60? POR SEGUNDO
isokflex60<- lm(DELTA_PTFLEX60 ~ DELTA_SMM + SMM + PTFLEX60, dados)
AIC(isokflex60)
par(mfrow=c(2,2))
plot(isokflex60)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(isokflex60$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(isokflex60))      #Observando a presen?a de outliers
car::durbinWatsonTest(isokflex60)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(isokflex60)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(isokflex60)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(isokflex60)                 #Resumo do modelo do supino
confint(isokflex60)                 #IC95%
par(mfrow=c(1,1))

#EXTENS?O ISOCIN?TICA A 180? POR SEGUNDO
isokext180<- lm(DELTA_PTEXT180 ~ DELTA_SMM, dados)
AIC(isokext180)
par(mfrow=c(2,2))
plot(isokext180)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(isokext180$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(isokext180))      #Observando a presen?a de outliers
car::durbinWatsonTest(isokext180)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(isokext180)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(isokext180)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(isokext180)                 #Resumo do modelo do supino
confint(isokext180)                 #IC95%
par(mfrow=c(1,1))

#FLEX?O ISOCIN?TICA A 180? POR SEGUNDO
isokflex180<- lm(DELTA_PTFLEX180 ~ DELTA_SMM + SMM + PTFLEX180, dados)
AIC(isokflex180)
par(mfrow=c(2,2))
plot(isokflex180)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(isokflex180$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(isokflex180))      #Observando a presen?a de outliers
car::durbinWatsonTest(isokflex180)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(isokflex180)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(isokflex180)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(isokflex180)                 #Resumo do modelo do supino
confint(isokflex180)                 #IC95%
par(mfrow=c(1,1))

#APTID?O FUNCIONAL
#VELOCIDADE DA MARCHA
velo_marcha<- lm(DELTA_VELO_MARCHA ~ DELTA_SMM + VELO_MARCHA, dados)
AIC(velo_marcha)
par(mfrow=c(2,2))
plot(velo_marcha)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(velo_marcha$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(velo_marcha))      #Observando a presen?a de outliers
car::durbinWatsonTest(velo_marcha)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(velo_marcha)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(velo_marcha)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(velo_marcha)                 #Resumo do modelo do supino
confint(velo_marcha)                 #IC95%
par(mfrow=c(1,1))


#AGILIDADE
agilidade<- lm(DELTA_AGILIDADE ~ DELTA_SMM + SMM + AGILIDADE, dados)
AIC(agilidade)
par(mfrow=c(2,2))
plot(agilidade)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(agilidade$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(agilidade))      #Observando a presen?a de outliers
car::durbinWatsonTest(agilidade)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(agilidade)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(agilidade)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(agilidade)                 #Resumo do modelo do supino
confint(agilidade)                 #IC95%
par(mfrow=c(1,1))


#SENTAR E LEVANTAR
sentar_levantar<- lm(DELTA_SENTAR_LEVANTAR ~ DELTA_SMM + SMM + SENTAR_LEVANTAR, dados)
AIC(sentar_levantar)
par(mfrow=c(2,2))
plot(sentar_levantar)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(sentar_levantar$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(sentar_levantar))      #Observando a presen?a de outliers
car::durbinWatsonTest(sentar_levantar)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(sentar_levantar)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(sentar_levantar)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(sentar_levantar)                 #Resumo do modelo do supino
confint(sentar_levantar)                 #IC95%
par(mfrow=c(1,1))

#CAMINHADA DE SEIS MINUTOS (regress?o simples tem tend?ncia, na m?ltipla, efeito desaparece)
seis_min<- lm(DELTA_CAMINHADA_6MIN ~ DELTA_SMM + SMM + CAMINHADA_6MIN, dados)
AIC(seis_min)
par(mfrow=c(2,2))
plot(seis_min)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(seis_min$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(seis_min))      #Observando a presen?a de outliers
car::durbinWatsonTest(seis_min)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(seis_min)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(seis_min)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(seis_min)                 #Resumo do modelo do supino
confint(seis_min)                 #IC95%
par(mfrow=c(1,1))


#ADIPOSIDADE CORPORAL
#GORDURA BRA?OS 
gordura_bracos<- lm(DELTA_GORDO_BRACOS ~ DELTA_SMM + SMM + GORDO_BRACOS, dados)
AIC(gordura_bracos)
par(mfrow=c(2,2))
plot(gordura_bracos)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(gordura_bracos$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(gordura_bracos))      #Observando a presen?a de outliers
car::durbinWatsonTest(gordura_bracos)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(gordura_bracos)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(gordura_bracos)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(gordura_bracos)                 #Resumo do modelo do supino
confint(gordura_bracos)                 #IC95%
par(mfrow=c(1,1))

#GORDURA PERNAS 
gordura_pernas<- lm(DELTA_GORDO_PERNAS ~ DELTA_SMM, dados)
AIC(gordura_pernas)
par(mfrow=c(2,2))
plot(gordura_pernas)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(gordura_pernas$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(gordura_pernas))      #Observando a presen?a de outliers
car::durbinWatsonTest(gordura_pernas)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(gordura_pernas)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(gordura_pernas)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(gordura_pernas)                 #Resumo do modelo do supino
confint(gordura_pernas)                 #IC95%
par(mfrow=c(1,1))

#GORDURA TRONCO 
gordura_tronco<- lm(DELTA_GORDO_TRONCO ~ DELTA_SMM + GORDO_TRONCO, dados)
AIC(gordura_tronco)
par(mfrow=c(2,2))
plot(gordura_tronco)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(gordura_tronco$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(gordura_tronco))      #Observando a presen?a de outliers
car::durbinWatsonTest(gordura_tronco)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(gordura_tronco)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(gordura_tronco)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(gordura_tronco)                 #Resumo do modelo do supino
confint(gordura_tronco)                 #IC95%
par(mfrow=c(1,1))


#GORDURA ANDROIDE 
gordura_androide<- lm(DELTA_GORDO_ANDROIDE ~ DELTA_SMM, dados)
AIC(gordura_androide)
par(mfrow=c(2,2))
plot(gordura_androide)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(gordura_androide$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(gordura_androide))      #Observando a presen?a de outliers
car::durbinWatsonTest(gordura_androide)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(gordura_androide)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(gordura_androide)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(gordura_androide)                 #Resumo do modelo do supino
confint(gordura_androide)                 #IC95%
par(mfrow=c(1,1))


#GORDURA GINOIDE 
gordura_ginoide<- lm(DELTA_GORDO_GINOIDE ~ DELTA_SMM, dados)
AIC(gordura_ginoide)
par(mfrow=c(2,2))
plot(gordura_ginoide)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(gordura_ginoide$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(gordura_ginoide))      #Observando a presen?a de outliers
car::durbinWatsonTest(gordura_ginoide)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(gordura_ginoide)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(gordura_ginoide)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(gordura_ginoide)                 #Resumo do modelo do supino
confint(gordura_ginoide)                 #IC95%
par(mfrow=c(1,1))


#GORDURA TOTAL 
gordura_total<- lm(DELTA_GORDO_TOTAL ~ DELTA_SMM + GORDO_TOTAL, dados)
AIC(gordura_total)
par(mfrow=c(2,2))
plot(gordura_total)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(gordura_total$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(gordura_total))      #Observando a presen?a de outliers
car::durbinWatsonTest(gordura_total)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(gordura_total)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(gordura_total)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(gordura_total)                 #Resumo do modelo do supino
confint(gordura_total)                 #IC95%
par(mfrow=c(1,1))

#RAZ?O GORDURA ANDROIDE/GINOIDE 
gordura_androide_ginoide<- lm(DELTA_R_ANDROIDE_GINOIDE ~ DELTA_SMM, dados)
AIC(gordura_androide_ginoide)
par(mfrow=c(2,2))
plot(gordura_androide_ginoide)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(gordura_androide_ginoide$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(gordura_androide_ginoide))      #Observando a presen?a de outliers
car::durbinWatsonTest(gordura_androide_ginoide)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(gordura_androide_ginoide)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(gordura_androide_ginoide)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(gordura_androide_ginoide)                 #Resumo do modelo do supino
confint(gordura_androide_ginoide)                 #IC95%
par(mfrow=c(1,1))



#PRESS?O ARTERIAL SIST?LICA
PAS<- lm(DELTA_PAS ~ DELTA_SMM + PAS + HIPERTENSAO, dados)
AIC(PAS)
par(mfrow=c(2,2))
plot(PAS)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(PAS$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(PAS))      #Observando a presen?a de outliers
car::durbinWatsonTest(PAS)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(PAS)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(PAS)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(PAS)                 #Resumo do modelo do supino
confint(PAS)                 #IC95%
par(mfrow=c(1,1))


#PRESS?O ARTERIAL DIAST?LICA
PAD<- lm(DELTA_PAD ~ DELTA_SMM + PAD + HIPERTENSAO, dados)
AIC(PAD)
par(mfrow=c(2,2))
plot(PAD)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(PAD$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(PAD))      #Observando a presen?a de outliers
car::durbinWatsonTest(PAD)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(PAD)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(PAD)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(PAD)                 #Resumo do modelo do supino
confint(PAD)                 #IC95%
par(mfrow=c(1,1))


#PRESS?O ARTERIAL M?DIA
PAM<- lm(DELTA_PAM ~ DELTA_SMM + PAM + HIPERTENSAO, dados)
AIC(PAM)
par(mfrow=c(2,2))
plot(PAM)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(PAM$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(PAM))      #Observando a presen?a de outliers
car::durbinWatsonTest(PAM)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(PAM)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(PAM)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(PAM)                 #Resumo do modelo do supino
confint(PAM)                 #IC95%
par(mfrow=c(1,1))

#GLICOSE
GLICOSE<- lm(DELTA_GLICOSE ~ DELTA_SMM + SMM + GLICOSE, dados)
AIC(GLICOSE)
par(mfrow=c(2,2))
plot(GLICOSE)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(GLICOSE$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(GLICOSE))      #Observando a presen?a de outliers
car::durbinWatsonTest(GLICOSE)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(GLICOSE)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(GLICOSE)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(GLICOSE)                 #Resumo do modelo do supino
confint(GLICOSE)                 #IC95%
par(mfrow=c(1,1))

#HBA1C
HBA1C<- lm(DELTA_HBA1C ~ DELTA_SMM + SMM + HBA1C + IMC, dados)
AIC(HBA1C)
par(mfrow=c(2,2))
plot(HBA1C)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(HBA1C$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(HBA1C))      #Observando a presen?a de outliers
car::durbinWatsonTest(HBA1C)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(HBA1C)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(HBA1C)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(HBA1C)                 #Resumo do modelo do supino
confint(HBA1C)                 #IC95%
par(mfrow=c(1,1))


#TRIGLICER?DEOS
TRIG<- lm(DELTA_TRIG ~ DELTA_SMM + TRIG, dados)
AIC(TRIG)
par(mfrow=c(2,2))
plot(TRIG)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(TRIG$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(TRIG))      #Observando a presen?a de outliers
car::durbinWatsonTest(TRIG)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(TRIG)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(TRIG)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(TRIG)                 #Resumo do modelo do supino
confint(TRIG)                 #IC95%
par(mfrow=c(1,1))


#COLESTEROL TOTAL
CT<- lm(DELTA_CT ~ DELTA_SMM + CT, dados)
AIC(CT)
par(mfrow=c(2,2))
plot(CT)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(CT$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(CT))      #Observando a presen?a de outliers
car::durbinWatsonTest(CT)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(CT)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(CT)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(CT)                 #Resumo do modelo do supino
confint(CT)                 #IC95%
par(mfrow=c(1,1))

#HDL
HDL<- lm(DELTA_HDL ~ DELTA_SMM + HDL, dados)
AIC(HDL)
par(mfrow=c(2,2))
plot(HDL)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(HDL$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(HDL))      #Observando a presen?a de outliers
car::durbinWatsonTest(HDL)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(HDL)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(HDL)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(HDL)                 #Resumo do modelo do supino
confint(HDL)                 #IC95%
par(mfrow=c(1,1))

#VLDL
VLDL<- lm(DELTA_VLDL ~ DELTA_SMM + VLDL, dados)
AIC(VLDL)
par(mfrow=c(2,2))
plot(VLDL)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(VLDL$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(VLDL))      #Observando a presen?a de outliers
car::durbinWatsonTest(VLDL)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(VLDL)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(VLDL)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(VLDL)                 #Resumo do modelo do supino
confint(VLDL)                 #IC95%
par(mfrow=c(1,1))

#LDL
LDL<- lm(DELTA_LDL ~ DELTA_SMM + LDL, dados)
AIC(LDL)
par(mfrow=c(2,2))
plot(LDL)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(LDL$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(LDL))      #Observando a presen?a de outliers
car::durbinWatsonTest(LDL)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(LDL)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(LDL)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(LDL)                 #Resumo do modelo do supino
confint(LDL)                 #IC95%
par(mfrow=c(1,1))


#PCR
PCR<- lm(DELTA_PCR ~ DELTA_SMM + SMM + PCR, dados)
AIC(PCR)
par(mfrow=c(2,2))
plot(PCR)                    #An?lise gr?fica dos presuspostos do modelo
shapiro.test(PCR$residuals)  #Normalidade dos res?duos pelo teste de Shapiro-Wilk
summary(rstandard(PCR))      #Observando a presen?a de outliers
car::durbinWatsonTest(PCR)   #Autocorrela??o dos res?duos, mas precisa tirar o na.action
lmtest::bptest(PCR)          #Homoscedasticidade pelo teste studentizado de Breusch-Pagan
vif(PCR)                     #Multicolinearidade: valores >10 s?o problem?ticos
summary(PCR)                 #Resumo do modelo do supino
confint(PCR)                 #IC95%
par(mfrow=c(1,1))


#Gr?ficos

grafico_gordo_ginoide<- ggplot2::ggplot(data = dados, 
              mapping = aes(x = DELTA_SMM,y = DELTA_GORDO_GINOIDE)) +
  geom_point() + geom_smooth(method = "lm", col = "red") + 
  stat_regline_equation((aes(label = paste(..eq.label.., ..adj.rr.label..,
                  sep = "*plain(\",\")~~"))), label.x = 3, label.y = 13) + 
  xlab("Changes in skeletal muscle mass (???%)") +  
  ylab ("Changes in gynoid fat (???%)") + theme_classic()

grafico_gordo_ginoide
ggsave("gynoid_fat.tiff", dpi = 600, width = 4.5, height = 3.15)


grafico_razao_androide_ginoide<- ggplot2::ggplot(data = dados, 
            mapping = aes(x = DELTA_SMM, y = DELTA_R_ANDROIDE_GINOIDE)) +
  geom_point() + geom_smooth(method = "lm", col = "red") + 
  stat_regline_equation((aes(label = paste(..eq.label.., ..adj.rr.label..,
  sep = "*plain(\",\")~~"))), label.x = 3.3, label.y = 17) + 
  xlab("Changes in skeletal muscle mass (???%)") +  
  ylab ("Changes in android:gynoid ratio") + 
  theme_classic()

grafico_razao_androide_ginoide
ggsave("android_gynoid_ratio.tiff", dpi = 600, width = 4, height = 2.8)
