
## Cicles econòmics
#importar fitxer (gdp_def)

data<-ts(gdp_def[,6:10],start=c(1990,1),frequency = 4)
lndata<-log(data)
colnames(lndata)<-c("NORLNY","DNKLNY", "SWELNY", "ISLNY", "FINLNY")
plot(lndata)

library(mFilter)

nf<-hpfilter(lndata[,1], freq=1600, type="lambda")
df<-hpfilter(lndata[,2],freq=1600, type="lambda")
sf<-hpfilter(lndata[,3],freq=1600, type="lambda")
isf<-hpfilter(lndata[,4],freq=1600, type="lambda")
fin<-hpfilter(lndata[,5],freq=1600, type="lambda")

cycles<-cbind(nf$cycle,df$cycle, sf$cycle, isf$cycle, fin$cycle)
plot(cycles)
cycles_df <- as.data.frame(cycles)
sapply(cycles_df, sd)

#calculem correlacions individuals

cor(cycles[,1],cycles[,2]) #NOR and SWE
cor(cycles[,1],cycles[,3]) #NOR and DEN
cor(cycles[,1],cycles[,4]) #NOR AND ISL
cor(cycles[,2],cycles[,3]) #SWE AND DEN
cor(cycles[,2],cycles[,4]) #SWE AND ISL
cor(cycles[,1],cycles[,2]) #DEN AND ISL
cor(cycles[,3],cycles[,4]) #DEN AND ISL
cor(cycles[,3],cycles[,4]) #DEN AND ISL
cor(cycles[,3],cycles[,4]) #DEN AND ISL

correlation <- data.frame(
  Norway = cycles[,1],
  Denmark = cycles[,2],
  Sweden = cycles[,3],
  Iceland = cycles[,4],
  Finland= cycles[,5]
)

# Matriu de correlacions
correlation_matrix <- cor(correlation)
print(round(correlation_matrix, 3))

# Apliquem ara el filtre BK

nf_2<-bkfilter(lndata[,1], pl=6, pu=32)
df_2<-bkfilter(lndata[,2], pl=6, pu=32)
sf_2<-bkfilter(lndata[,3], pl=6, pu=32)
isf_2<-bkfilter(lndata[,4], pl=6, pu=32)
fin_2<-bkfilter(lndata[,5], pl=6, pu=32)

cycles2<-cbind(nf_2$cycle,df_2$cycle, sf_2$cycle, isf_2$cycle, fin_2$cycle)
cycles2=na.omit(cycles2)
plot(cycles2)

#calcular la desv.pels cicles
cycles2_df <- as.data.frame(cycles2)
sapply(cycles2_df, sd)

#calculem correlacions individuals

cor(cycles2[,1],cycles2[,2]) #NOR and SWE
cor(cycles2[,1],cycles2[,3]) #NOR and DEN
cor(cycles2[,1],cycles2[,4]) #NOR AND ISL
cor(cycles2[,2],cycles2[,3]) #SWE AND DEN
cor(cycles2[,2],cycles2[,4]) #SWE AND ISL
cor(cycles2[,1],cycles2[,2]) #DEN AND ISL
cor(cycles2[,3],cycles2[,4]) #DEN AND ISL
cor(cycles2[,3],cycles2[,4]) #DEN AND ISL
cor(cycles2[,3],cycles2[,4]) #DEN AND ISL

correlation2 <- data.frame(
  Norway = cycles2[,1],
  Denmark = cycles2[,2],
  Sweden = cycles2[,3],
  Iceland = cycles2[,4],
  Finland= cycles2[,5]
)

# Matriu de correlacions
correlation_matrix2 <- cor(correlation2)
print(round(correlation_matrix2, 3))

# Gràfic
par(mfrow = c(2, 1))
ts.plot(cycles[,1],cycles[,2],cycles[,3], cycles[,4],cycles[,5], gpars=list(xlab="Any", main="Filtre HP"), col=c("black","green", "red", "blue", "purple"), lwd="1.5")
abline(h = 0,lwd=2,lty = "dashed", col = "grey")
legend("bottomright", inset = c(0.08,-0.75),horiz=TRUE,cex = 1.2, legend=c("NOR","DNK", "SWE", "ISL", "FIN"),bty = "n",bg="transparent",lwd=c(2,2),col=c("black","green", "red", "blue", "purple"))
ts.plot(cycles2[,1],cycles2[,2],cycles2[,3], cycles2[,4],cycles2[,5], gpars=list(xlab="Any", main="Filtre BK"), col=c("black","green", "red", "blue", "purple"), lwd="1.5")
abline(h = 0,lwd=2 ,lty = "dashed", col = "grey")
legend("bottomright", inset = c(0.08,-0.75),horiz=TRUE,cex = 1.2, legend=c("NOR","DNK", "SWE", "ISL", "FIN"),bty = "n",bg="transparent",lwd=c(2,2),col=c("black","green", "red", "blue", "purple"))
par(new=FALSE)



#--------


  
# Model SVAR

# Importem llibreries
library(forecast)
library(vars)
library(tseries)

# Convertim les dades a sèries temporals
gdp_den=ts(gdp_def[,7], start(1990,1), freq=4)
def_den=ts(gdp_def[,5], start(1990,1), freq=4)

gdp_nor=ts(gdp_def[,6], start(1990,1), freq=4)
def_nor=ts(gdp_def[,4], start(1990,1), freq=4)

gdp_swe=ts(gdp_def[,8], start(1990,1), freq=4)
def_swe=ts(gdp_def[,2], start(1990,1), freq=4)

gdp_isl=ts(gdp_def[,9], start(1990,1), freq=4)
def_isl=ts(gdp_def[,3], start(1990,1), freq=4)

gdp_fin=ts(gdp_def[,10], start(1990,1), freq=4)
def_fin=ts(gdp_def[,11], start(1990,1), freq=4)

# Convertim en logaritmes
lgdp_den=log(gdp_den)
ldef_den=log(def_den)

lgdp_nor=log(gdp_nor)
ldef_nor=log(def_nor)

lgdp_swe=log(gdp_swe)
ldef_swe=log(def_swe)

lgdp_isl=log(gdp_isl)
ldef_isl=log(def_isl)

lgdp_fin=log(gdp_fin)
ldef_fin=log(def_fin)

# Gràfic
par(mfrow=c(1,2))
ts.plot(lgdp_den,lgdp_nor,lgdp_swe, lgdp_isl,lgdp_fin, col=c("red", "blue", "green", "black", "purple"))
ts.plot(ldef_den,ldef_nor,ldef_swe, ldef_isl,ldef_fin, col=c("red", "blue", "green", "black", "purple"))
par(mfrow=c(1,1))

#extraiem el el nombre convenient de diferències (segons ADF)

ndiffs(lgdp_den, test="adf")
ndiffs(ldef_den, test="adf")

ndiffs(lgdp_nor,test="adf")
ndiffs(ldef_nor,test="adf")

ndiffs(lgdp_swe, test="adf")
ndiffs(ldef_swe, test="adf")

ndiffs(lgdp_isl, test="adf")
ndiffs(ldef_isl, test="adf")

ndiffs(lgdp_fin, test="adf")
ndiffs(ldef_fin, test="adf")

# diferenciem
dlgdp_den=diff(lgdp_den)
adf.test(dlgdp_den) #(per comprovar la estacionaritat)

dlgdp_nor=diff(lgdp_nor)
adf.test(dlgdp_nor)

dlgdp_swe=diff(lgdp_swe)
adf.test(dlgdp_swe)

dlgdp_isl=diff(lgdp_isl)
adf.test(dlgdp_isl)

dlgdp_fin=diff(lgdp_fin)
adf.test(dlgdp_fin)

#Gràfic
ts.plot(dlgdp_den,dlgdp_nor,dlgdp_swe,dlgdp_isl,dlgdp_fin,col=c("red", "blue", "green", "black", "purple"))


#Ara fem el mateix amb el deflactor

dldef_den=diff(ldef_den)
adf.test(dldef_den)

dldef_nor=diff(ldef_nor)
adf.test(dldef_nor)

dldef_swe=diff(ldef_swe)
adf.test(dldef_swe)

dldef_isl=diff(ldef_isl)
adf.test(dldef_isl)

dldef_fin=diff(ldef_fin)
adf.test(dldef_fin)


ts.plot(dldef_den,dldef_nor,dldef_swe,dldef_isl, dldef_fin, col=c("red", "blue", "green", "black", "purple"))


# Grafiquem
par(mfrow=c(3,2))
ts.plot(dlgdp_den,dldef_den,col=c("blue", "red"))
ts.plot(dlgdp_swe,dldef_swe,col=c("blue", "red"))
ts.plot(dlgdp_nor,dldef_nor,col=c("blue", "red"))
ts.plot(dlgdp_isl,dldef_isl,col=c("blue", "red"))
ts.plot(dlgdp_fin,dldef_fin,col=c("blue", "red"))
par(mfrow=c(1,1))

#Convertim en sèries temporals les dades diferenciades
vardlgdp_den=ts(dlgdp_den, start=1990, freq=4)
vardlgdp_nor=ts(dlgdp_nor, start=1990, freq=4)
vardlgdp_swe=ts(dlgdp_swe, start=1990, freq=4)
vardlgdp_isl=ts(dlgdp_isl, start=1990, freq=4)
vardlgdp_fin=ts(dlgdp_fin, start=1990, freq=4)

vardldef_den=ts(dldef_den, start=1990, freq=4)
vardldef_nor=ts(dldef_nor, start=1990, freq=4)
vardldef_swe=ts(dldef_swe, start=1990, freq=4)
vardldef_isl=ts(dldef_isl, start=1990, freq=4)
vardldef_fin=ts(dldef_fin, start=1990, freq=4)

#Combinem les dues sèries temporals (PIB i deflactor)
ejvar=cbind(vardlgdp_den,vardldef_den)
print(ejvar)

ejvar2=cbind(vardlgdp_nor,vardldef_nor)
print(ejvar2)

ejvar3=cbind(vardlgdp_swe,vardldef_swe)
print(ejvar3)

ejvar4=cbind(vardlgdp_isl,vardldef_isl)
print(ejvar4)

ejvar5=cbind(vardlgdp_fin,vardldef_fin)
print(ejvar5)

# Seleccionem el lag
VARselect(ejvar, lag.max = 10, type="const")
VARselect(ejvar2, lag.max = 10)
VARselect(ejvar3, lag.max = 10)
VARselect(ejvar4, lag.max = 10)
VARselect(ejvar5, lag.max = 10)

#executem el model VAR amb el lag (en aquest cas 1)
var1=VAR(ejvar,p=1)
summary(var1)

var2=VAR(ejvar2,p=1)
summary(var2)

var3=VAR(ejvar3,p=1)
summary(var3)

var4=VAR(ejvar4,p=1)
summary(var4)

var5=VAR(ejvar5,p=1)
summary(var5)

# emmagatzemem els residus
residuals_country1_v <- residuals(var1)
residuals_country2_v <- residuals(var2)
residuals_country3_v <- residuals(var3)
residuals_country4_v <- residuals(var4)
residuals_country5_v <- residuals(var5)

#Computem el model SVAR (amb la funció BQ())

BQ.den=BQ(var1)
BQ.nor=BQ(var2)
BQ.swe=BQ(var3)
BQ.isl=BQ(var4)
BQ.fin=BQ(var5)

#obtenim els residus estructurals de la següent expressió:

#u=B⋅ϵ (estructurals = B* reduïts)

structural_shocks_den <-  BQ.den$B %*% t(residuals_country1_v)
structural_shocks_nor <-  BQ.nor$B %*% t(residuals_country2_v)
structural_shocks_swe <-  BQ.swe$B %*% t(residuals_country3_v)
structural_shocks_isl <-  BQ.isl$B %*% t(residuals_country4_v)
structural_shocks_fin <-  BQ.fin$B %*% t(residuals_country5_v)

structural_shocks_den_transf <-t(as.matrix(structural_shocks_den))
structural_shocks_nor_transf <-t(as.matrix(structural_shocks_nor))
structural_shocks_swe_transf <-t(as.matrix(structural_shocks_swe))
structural_shocks_isl_transf <-t(as.matrix(structural_shocks_isl))
structural_shocks_fin_transf <-t(as.matrix(structural_shocks_fin))

residuals_country1_transf <- structural_shocks_den_transf[,1]
residuals_country2_transf <- structural_shocks_nor_transf[,1]
residuals_country3_transf <- structural_shocks_swe_transf[,1]
residuals_country4_transf <- structural_shocks_isl_transf[,1]
residuals_country5_transf <- structural_shocks_fin_transf[,1]

residuals_country1_transf2 <- structural_shocks_den_transf[,2]
residuals_country2_transf2 <- structural_shocks_nor_transf[,2]
residuals_country3_transf2 <- structural_shocks_swe_transf[,2]
residuals_country4_transf2 <- structural_shocks_isl_transf[,2]
residuals_country5_transf2 <- structural_shocks_fin_transf[,2]

#emmagatzemem els residus tan de la oferta com la demanda
residuals_data_s <- data.frame(
  Denmark = residuals_country1_transf,
  Norway = residuals_country2_transf,
  Sweden = residuals_country3_transf,
  Iceland = residuals_country4_transf,
  Finland= residuals_country5_transf
)

residuals_data_d <- data.frame(
  Denmark = residuals_country1_transf2,
  Norway = residuals_country2_transf2,
  Sweden = residuals_country3_transf2,
  Iceland = residuals_country4_transf2,
  Finland= residuals_country5_transf2
)

# Computem la matriu de correlacions (oferta)
correlation_matrix_s <- cor(residuals_data_s)
print(round(correlation_matrix_s, 3))

# Computem la matriu de correlacions (demanda)
correlation_matrix_d <- cor(residuals_data_d)
print(round(correlation_matrix_d, 3))




  
