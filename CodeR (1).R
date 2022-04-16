
## Récuperation des données

data <- read.csv("C:/Users/lg/Downloads/Electric_Production.csv", header=TRUE)
head(data,24)
summary(data)

## Vérification de l'existence des valeurs manquantes

sum(is.na(data))

## Construction d'une série chronologique à la base des données dont on se dispose

sc <- ts(data$Value, start=c(1985, 1), end=c(2018, 1), frequency=12)
sc

## Représentation graphique de la série chronologique

plot(sc, type='l',col='red')

## Passage d'un modèle multiplicatif à un modèle additif

scStat <- log(sc)
plot(scStat, type='l',col='red')


## Division des données en un échantillon d'apprentissage et un échantillon de validation

sctrain <- window(scStat,end=c(2008,12))
sctest <- window(scStat,start=c(2009,1))

## Détection de la tendance et de la saisonnalité 

plot(sctrain, type='l',col='red')
plot(decompose(sctrain))
decompose(sctrain)$seasonal
decompose(sctrain)$trend


## les corrélogrammes simple et partiel de la série 

par(mfrow=c(2,1)) 
plot(acf(sctrain,lag.max=36,plot=FALSE),ylim=c(-1,1))
plot(pacf(sctrain,lag.max=36,plot=FALSE),ylim=c(-1,1))


## Tests de stationnarité

adf.test(sctrain)
kpss.test(sctrain)
pp.test(sctrain)
PP.test(sctrain)

## Différenciation (I-B) de la série(élimination de la tendance)


sc_diff1=diff(sctrain,lag=1,differences=1)
par(mfrow=c(2,1)) 
plot(acf(sc_diff1,lag.max=36,plot=FALSE),ylim=c(-1,1))
plot(pacf(sc_diff1,lag.max=36,plot=FALSE),ylim=c(-1,1))


########## Tests de stationnarité ###########


adf.test(sc_diff1)
kpss.test(sc_diff1)
pp.test(sc_diff1)
PP.test(sc_diff1)

## Différenciation (I-B^12) saisonniére 

sc_diff2=diff(sc_diff1,lag=12,differences=1)
par(mfrow=c(2,1)) 
plot(acf(sc_diff2,lag.max=36,plot=FALSE),ylim=c(-1,1))
plot(pacf(sc_diff2,lag.max=36,plot=FALSE),ylim=c(-1,1))


########## Tests de stationnarité ###########

adf.test(sc_diff2)
kpss.test(sc_diff2)
pp.test(sc_diff2)
PP.test(sc_diff2)

## Identification , estimation et validation des modèles 

### Modèle 1

model1=arima(sctrain,order=c(3,1,2),list(order=c(1,1,2),period=12), include.mean=FALSE,method="CSS-ML")
coeftest(model1)


### Modèle 2

model2=arima(sctrain,order=c(2,1,3),list(order=c(1,1,2),period=12),include.mean=FALSE,method="CSS-ML")
coeftest(model2)


### Modèle 3

model3=arima(sctrain,order=c(1,1,1),list(order=c(0,1,2),period=12),include.mean=FALSE,method="CSS-ML")
coeftest(model3)


### Modèle 4

model4=arima(sctrain,order=c(1,1,1),list(order=c(1,1,2),period=12),include.mean=FALSE,method="CSS-ML")
coeftest(model4)


## Validation du Modèle4 à l'aide des méthodes Box et Jenkins

Box.test(model4$residuals,type="Ljung-Box")
Box.test(model4$residuals,type="Box-Pierce")
Box.test(model4$residuals,type="Box-Pierce")$p.value


## Test de normalité des résidus du modèle4

shapiro.test(model4$residuals)
jarque.bera.test(model4$residuals)

## Prévision

pred_model4=predict(model4,n.ahead=108)
plot(pred_model4$pred , col="red")
lines(sctest, col="blue")

## RMSE et MAPE

rmse=sqrt(mean((sctest - pred_model4$pred)^2))
rmse
mape=mean(abs(1-pred_model4$pred/sctest))*100
mape

## modélisation automatique

auto.arima(sctrain, trace=TRUE)























