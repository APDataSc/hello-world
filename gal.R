#**************************************************************************************#
#**************************************************************************************#
#
#              CARACTERIZACIÓN  DE LOS HOGARES QUE PERCIBEN LA CFB (ENIGHUR 11-12)                         
#
#     Responsable:            Andrés Peña M. 
#     Fecha de elaboración:   02/03/2017
#     Última actualización:   06/02/2017
#     Modificado por:         Andrés Peña M. 
#
#**************************************************************************************#
#**************************************************************************************#

#Chapter I - Cleaning data 

rm(list=ls())

#1. Packages
#install.packages(c("foreign", "survey", "calibrate"), dependencies = TRUE)
library(foreign)
library(survey)
library(dplyr)


#choose.files()
file<-"C:\\Users\\apena\\Desktop\\Tesis\\CEPAL 2017_INEC\\V3\\Cap3\\data\\Tablas_Trabajo\\02 TABLAS DE TRABAJO"
setwd(file)  

#lectura de base de datos en formato SPSS
hog_agr<-read.spss("10 ENIGHUR11_HOGARES_AGREGADOS.sav", use.value.labels = T,
                   to.data.frame = T)

#solo variables necesarias
bdd<-hog_agr%>%select(Identif_2010, Identif_hog, Fexp_cen2010, Provincia, 
                      Área, sexo, edad, estado, escolaridad, instruccion,
                      grupos_edad, rama_p, grupo_p, cateocu_p, horas_total,
                      asegurado, ocupados, perceptor, per_ocu, numpers,
                      d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12,
                      gas_gru_cor, ing_cor_per)

#diseño muestral original
design<-svydesign(data=bdd, id=~Identif_2010,
                  strata=~Provincia, weights=~Fexp_cen2010)

#área urbana
design_gal<-subset(design, Provincia=="Galápagos")


#percentiles del área urbana
percentiles<- as.vector(svyquantile(~ing_cor_per, quantile= c(0, 0.33, 0.66, 1),
                                    design=design_gal, deff=TRUE, na.rm=TRUE, ties="rounded"))
percentiles #Promedio de la CFB de jul-2011 a jun-2012: $576.14, percentil 34

#ubicación de percentiles en la base - "pendiente"
bdd<-bdd%>%filter(Provincia=="Galápagos")
bdd$perc <- cut(bdd$ing_cor_per, breaks=percentiles, 
                labels=FALSE, include.lowest=TRUE)

bdd$per_val<-ifelse(bdd$perc>0,1,NA) #casos válidos

#galápagos
design<-svydesign(data=bdd, id=~Identif_2010,
                  strata=~Provincia, weights=~Fexp_cen2010)
design_gal<-subset(design, Provincia=="Galápagos")


#------------------------------------------------------------------

#Análisis de los hogares 

#Sexo del jefe del hogar
svyby(~sexo, ~per_val, design_gal, svytotal, vartype =c("cv"))
sexo_jefe<-svytotal(~sexo, na.rm=TRUE, design_gal)
sexo_jefe_cv<-cv(sexo_jefe)
sexo_jefe_ci<-confint(sexo_jefe)
sexo<-cbind(sexo_jefe, sexo_jefe_cv, sexo_jefe_ci)
barplot(sexo_jefe/casos[1], col = "peachpuff")
write.table(sexo, "clipboard", sep="\t", row.names=TRUE, dec = ",")


#Estado Civil del Jefe de Hogar
svyby(~estado, ~per_val, design_gal, svytotal, vartype =c("cv"))
estado_jefe<-svytotal(~estado, na.rm=TRUE, design_gal)
estado_jefe_cv<-cv(estado_jefe)
estado_jefe_ci<-confint(estado_jefe)
estado<-cbind(estado_jefe, estado_jefe_cv, estado_jefe_ci)
barplot(estado_jefe/casos[1], col = "peachpuff")
write.table(estado, "clipboard", sep="\t", row.names=TRUE, dec = ",")


#Escolaridad del Jefe de Hogar
escolaridad_jefe_mean<-svymean(~escolaridad, na.rm=TRUE, design_gal)
escolaridad_jefe_per<-svyquantile(~escolaridad, quantile= c(0.25, 0.5, 0.75),
                                  design_gal, na.rm=TRUE, ties="rounded")
escolaridad_jefe_cv<-cv(escolaridad_jefe_mean)
escolaridad<-cbind(escolaridad_jefe_mean, escolaridad_jefe_per, escolaridad_jefe_cv)

svyhist(~escolaridad, design_gal, main = "", col = "peachpuff")
svyboxplot(escolaridad ~ sexo, design_gal, col = "pink")
write.table(escolaridad, "clipboard", sep="\t", row.names=F, dec = ",")


#Nivel de Instrucción del Jefe de Hogar
svyby(~instruccion, ~per_val, design_gal, svytotal, vartype =c("cv"))
instruccion_jefe<-svytotal(~instruccion, na.rm=TRUE, design_gal)
instruccion_jefe_cv<-cv(instruccion_jefe)
instruccion_jefe_ci<-confint(instruccion_jefe)
instruccion<-cbind(instruccion_jefe, instruccion_jefe_cv, instruccion_jefe_ci)
barplot(instruccion_jefe/casos[1], col = "peachpuff")
write.table(instruccion, "clipboard", sep="\t", row.names=TRUE, dec = ",")


#Rama de Actividad de la Ocupación Principal del Jefe del Hogar
svyby(~rama_p, ~per_val, design_gal, svytotal, vartype =c("cv"))
rama_p_jefe<-svytotal(~rama_p, na.rm=TRUE, design_gal)
rama_p_jefe_cv<-cv(rama_p_jefe)
rama_p_jefe_ci<-confint(rama_p_jefe)
rama_p<-cbind(rama_p_jefe, rama_p_jefe_cv, rama_p_jefe_ci)
barplot(rama_p_jefe/casos[1], col = "peachpuff")
write.table(rama_p, "clipboard", sep="\t", row.names=TRUE, dec = ",")


#Grupo de Ocupación Principal del Jefe del Hogar
svyby(~grupo_p, ~per_val, design_gal, svytotal, vartype =c("cv"))
grupo_p_jefe<-svytotal(~grupo_p, na.rm=TRUE, design_gal)
grupo_p_jefe_cv<-cv(grupo_p_jefe)
grupo_p_jefe_ci<-confint(grupo_p_jefe)
grupo_p<-cbind(grupo_p_jefe, grupo_p_jefe_cv, grupo_p_jefe_ci)
barplot(grupo_p_jefe/casos[1], col = "peachpuff")
write.table(grupo_p, "clipboard", sep="\t", row.names=TRUE, dec = ",")


#Categoría de Ocupación Principal del Jefe del Hogar
svyby(~cateocu_p, ~per_val, design_gal, svytotal, vartype =c("cv"))
cateocu_p_jefe<-svytotal(~cateocu_p, na.rm=TRUE, design_gal)
cateocu_p_jefe_cv<-cv(cateocu_p_jefe)
cateocu_p_jefe_ci<-confint(cateocu_p_jefe)
cateocu_p<-cbind(cateocu_p_jefe, cateocu_p_jefe_cv, cateocu_p_jefe_ci)
barplot(cateocu_p_jefe/casos[1], col = "peachpuff")
write.table(cateocu_p, "clipboard", sep="\t", row.names=TRUE, dec = ",")


#Horas Totales de Trabajo del Jefe del Hogar
horas_total_jefe_mean<-svymean(~horas_total, na.rm=TRUE, design_gal)
horas_total_jefe_per<-svyquantile(~horas_total, quantile= c(0.25, 0.5, 0.75),
                                  design_gal, na.rm=TRUE, ties="rounded")
horas_total_jefe_cv<-cv(horas_total_jefe_mean)
horas_total<-cbind(horas_total_jefe_mean, horas_total_jefe_per, horas_total_jefe_cv)

svyhist(~horas_total, design_gal, main = "", col = "peachpuff")
svyboxplot(horas_total ~ sexo, design_gal, col = "pink")
write.table(horas_total, "clipboard", sep="\t", row.names=F, dec = ",")


#Seguridad Pública y Privada del Jefe del Hogar
svyby(~asegurado, ~per_val, design_gal, svytotal, vartype =c("cv"))
asegurado_jefe<-svytotal(~asegurado, na.rm=TRUE, design_gal)
asegurado_jefe_cv<-cv(asegurado_jefe)
asegurado_jefe_ci<-confint(asegurado_jefe)
asegurado<-cbind(asegurado_jefe, asegurado_jefe_cv, asegurado_jefe_ci, casos[1])
barplot(asegurado_jefe/casos[1], col = "peachpuff")
write.table(asegurado, "clipboard", sep="\t", row.names=TRUE, dec = ",")


#Población Ocupada
ocupados_hogar_mean<-svymean(~ocupados, na.rm=TRUE, design_gal)
ocupados_hogar_per<-svyquantile(~ocupados, quantile= c(0.25, 0.5, 0.75),
                                design_gal, na.rm=TRUE, ties="rounded")
ocupados_hogar_cv<-cv(ocupados_hogar_mean)
ocupados<-cbind(ocupados_hogar_mean, ocupados_hogar_per, ocupados_hogar_cv)

svyhist(~ocupados, design_gal, main = "", col = "peachpuff")
svyboxplot(ocupados ~ sexo, design_gal, col = "pink")
write.table(ocupados, "clipboard", sep="\t", row.names=F, dec = ",")

#Perceptor de Ingresos
perceptor_hogar_mean<-svymean(~perceptor, na.rm=TRUE, design_gal)
perceptor_hogar_per<-svyquantile(~perceptor, quantile= c(0.25, 0.5, 0.75),
                                 design_gal, na.rm=TRUE, ties="rounded")
perceptor_hogar_cv<-cv(perceptor_hogar_mean)
perceptor<-cbind(perceptor_hogar_mean, perceptor_hogar_per, perceptor_hogar_cv)

svyhist(~perceptor, design_gal, main = "", col = "peachpuff")
svyboxplot(perceptor ~ sexo, design_gal, col = "pink")
write.table(perceptor, "clipboard", sep="\t", row.names=F, dec = ",")


#Perceptor de Ingresos Ocupado
per_ocu_hogar_mean<-svymean(~per_ocu, na.rm=TRUE, design_gal)
per_ocu_hogar_per<-svyquantile(~per_ocu, quantile= c(0.25, 0.5, 0.75),
                               design_gal, na.rm=TRUE, ties="rounded")
per_ocu_hogar_cv<-cv(per_ocu_hogar_mean)
per_ocu<-cbind(per_ocu_hogar_mean, per_ocu_hogar_per, per_ocu_hogar_cv)

svyhist(~per_ocu, design_gal, main = "", col = "peachpuff")
svyboxplot(per_ocu ~ sexo, design_gal, col = "pink")
write.table(per_ocu, "clipboard", sep="\t", row.names=F, dec = ",")


#Tamaño del Hogar
numpers_hogar_mean<-svymean(~numpers, na.rm=TRUE, design_gal)
numpers_hogar_per<-svyquantile(~numpers, quantile= c(0.25, 0.5, 0.75),
                               design_gal, na.rm=TRUE, ties="rounded")
numpers_hogar_cv<-cv(numpers_hogar_mean)
numpers<-cbind(numpers_hogar_mean, numpers_hogar_per, numpers_hogar_cv)

svyhist(~numpers, design_gal, main = "", col = "peachpuff")
svyboxplot(numpers ~ sexo, design_gal, col = "pink")
write.table(numpers, "clipboard", sep="\t", row.names=F, dec = ",")


#Divisiones de gasto de consumo
gasto_hogar_mean<-svymean(~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+gas_gru_cor,
                          na.rm=TRUE, design_gal)
gasto_hogar_total<-svytotal(~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+gas_gru_cor,
                            na.rm=TRUE, design_gal)
gasto_hogar_per<-svyquantile(~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+gas_gru_cor,
                             quantile= c(0.25, 0.5, 0.75),
                             design_gal, na.rm=TRUE, ties="rounded")
gasto_hogar_cv<-cv(gasto_hogar_mean)
gasto_hogar_cvt<-cv(gasto_hogar_total)
gasto<-cbind(gasto_hogar_mean, gasto_hogar_total, gasto_hogar_per, 
             gasto_hogar_cv, gasto_hogar_cvt)

svyhist(~gas_gru_cor, design_gal, main = "", col = "peachpuff")
svyboxplot(~gas_gru_cor ~ sexo, design_gal, col = "pink")
write.table(gasto, "clipboard", sep="\t", row.names=F, dec = ".")

gasto_hogar_total<-svytotal(~d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+gas_gru_cor,
                            na.rm=TRUE, design_gal)

svyby(~gas_gru_cor, ~perc, design_gal, svytotal, vartype =c("cv"))
svyby(~d1, ~perc, design_gal, svytotal, na.rm=TRUE, vartype =c("cv"))


#**************************************************************************************#
#**************************************************************************************#
#
#              COMPONENTE ALIMENTARIO CFB (ENIGHUR 11-12)                         
#
#     Responsable:            Andrés Peña M. 
#     Fecha de elaboración:   11/03/2017
#     Última actualización:   11/03/2017
#     Modificado por:         Andrés Peña M. 
#
#**************************************************************************************#
#**************************************************************************************#

#Chapter I - Cleaning data 

rm(list=ls())

#1. Packages
install.packages(c("foreign", "survey", "calibrate", "ggplot2"), dependencies = TRUE)
library(foreign)
library(survey)
library(dplyr)
library(ggplot2)
library(srvyr)

#choose.files()
#file<-"C:\\Users\\Andres\\Desktop\\CEPAL 2017_INEC\\V3\\Cap3\\data\\Tablas_Trabajo\\02 TABLAS DE TRABAJO"
file<-"C:\\Users\\apena\\Desktop\\CEPAL 2017_INEC\\V3\\Cap3\\data\\Tablas_Trabajo\\02 TABLAS DE TRABAJO"
setwd(file)  

#lectura de base de datos en formato SPSS
gastos<-read.spss("06 ENIGHUR11_GASTOS_V.sav", use.value.labels = T,
                  to.data.frame = T)


#Cálculo de la variable salario

#solo variables necesarias
bdd<-gastos%>%select(Identif_2010, Identif_hog, Fexp_cen2010, Provincia, 
                     Área, nprod, codciif, gastomo, cantidad, unidad,      
                     frecuen, forma, pago, adquirio, consumo, destino)

bdd$ccif<-substr(bdd$codciif,1,7)
bdd$largo<-nchar(trimws(bdd$ccif))
bdd$division<-ifelse(bdd$largo==6, substr(bdd$codciif,1,1), substr(bdd$codciif,1,2))

bdd_ali<-filter(bdd, division==1)

table(bdd_ali$unidad)
table(bdd_ali$codciif)



#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
grafico<-q[,c(6,11:13)]
grafico$cod<-substr(grafico$codciif, 8, 15)
grafico$ccif<-substr(grafico$codciif, 1, 6)
grafico<-grafico%>%arrange(desc(freq))%>%
  filter(ccif!="122001" &
           ccif!="122002" &
           ccif!="122007" &
           ccif!="122009" &
           ccif!="122102" &
           ccif!="118304" &
           ccif!="118307" &
           ccif!="11921")
grafico<-grafico[c(1:10),]


ggplot(data = grafico, aes(x = reorder(cod, -freq), y = freq, fill = freq,
                           ymax = freq_ls, ymin = freq_li)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = paste0(round(freq*100,1),"%")), 
            position = position_dodge(width = 0.5), vjust = -8) +
  geom_hline(yintercept=c(0.2), linetype="dashed", color = "red") +
  theme(legend.position="none") +
  #  theme(axis.title.x=element_blank()) +
  labs(x="Alimentos",y="Frecuencia de hogares")


#--------------------------------------------------------------

grafico<-q[,c(6,11:13)]
grafico$cod<-substr(grafico$codciif, 8, 15)
grafico$ccif<-substr(grafico$codciif, 1, 6)
grafico<-grafico%>%
  filter(ccif=="119132" |
           ccif=="116051" |
           ccif=="117027" |
           ccif=="117018" |
           ccif=="117005" |
           ccif=="117101" |
           ccif=="119010" |
           ccif=="112315" |
           ccif=="116064" |
           ccif=="117103")%>%
  arrange(desc(freq))

ggplot(data = grafico, aes(x = reorder(cod, -freq), y = freq, fill = freq,
                           ymax = freq_ls, ymin = freq_li)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0.5, label = paste0(round(freq*100,1),"%")), 
            position = position_dodge(width = 0.5), vjust = 20) +
  geom_hline(yintercept=c(0.2), linetype="dashed", color = "red") +
  theme(legend.position="none") +
  #  theme(axis.title.x=element_blank()) +
  labs(x="Alimentos",y="Frecuencia de hogares")