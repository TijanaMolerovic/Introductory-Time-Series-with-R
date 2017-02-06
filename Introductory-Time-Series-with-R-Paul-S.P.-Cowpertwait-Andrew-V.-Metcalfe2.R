#prvi deo
#literatura: Introductory Time Series With R


#5.3
#koristi se electricity production serija(1958-1990)
www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/cbe.dat"
CBE <- read.table(www, header = TRUE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
plot(Elec.ts, main = "", ylab = "Electricity production / MkWh") #jasan trend i sezonska komponenta, disperzija raste
logElec.ts<-log(Elec.ts)
plot(logElec.ts, main = "", ylab = "log(Electricity production) / MkWh")
#a)Zasto je log transformacija prikladna za ovu seriju?
#na osnovu grafika ove serije deluje da ona ima priblizno eksponencijalni trend i 
#kao i da varijabilnost podataka raste sa vremenom
#za oba ova "problema" logaritmovanje moze da posluzi kao resenje
#takodje log transformacijom prelazimo sa multiplikativnog modela na aditivni jer vazi
#x_t=m_t*s_t*z_t  y_t=log(x_t)=log(m_t)+log(s_t)+log(z_t), log transformacijo stabilizujemo disperziju
#sa drugog grafika se moze primetiti da je logarimovanje stabilizovalo disperziju i sad mozemo da pretpostavimo da je konstantna tokom vremena
#nadalje posmatramo seriju logElec.ts

#b)fituj sezonski indikatorski model sa kvadratnim trendom za prirodni logaritam ove serije.
vreme<-time(logElec.ts);sezone<-cycle(logElec.ts) #ali koristicemo standardizovane vrednosti za vreme iz razloga dobijanja vecih koeficijenata
vreme<-(vreme-mean(vreme))/sd(vreme)
sim.lm<-lm(logElec.ts~0+vreme+I(vreme^2)+factor(sezone))#sezonski indikatorski model sa kvadratnim trendom
#0 da bi se ocenjene sve sezonske komponente
#koristimo stepwise regresiju za izbor najboljeg modela koji je zasnovan na AIC kriterijumu
step(sim.lm)
#pocetni model je bolji od ostalih modela, i kod njega AIC=-2717.56 (najmanji)

#c)fituj harmonijski model sa kvadratnim trendom za prirodni logaritam ove serije.
vreme<-time(logElec.ts);sezone<-cycle(logElec.ts)
SIN<-COS<-matrix(nrow=length(vreme),ncol = 6)
for(i in 1:6)
{
  SIN[,i]=sin(2*pi*i*vreme)
  COS[,i]=cos(2*pi*i*vreme)
}
vreme<-(vreme-mean(vreme))/sd(vreme)
harm.lm<-lm(logElec.ts~vreme+I(vreme^2)+SIN[,1]+COS[,1]+SIN[,2]+COS[,2]+SIN[,3]+COS[,3]+SIN[,4]+COS[,4]+SIN[,5]+COS[,5]+SIN[,6]+COS[,6])#harmonijski model sa kvadratnim trendom
#koristimo opet stepwise regresiju za izbor najboljeg modela koji je zasnovan na AIC kriterijumu
step(harm.lm) #izabran je sledeci model, sa AIC=-2724.49
harm.lm<-lm(logElec.ts ~ vreme + I(vreme^2) + SIN[,1] + COS[,1] + SIN[,2] + COS[,2] + COS[,3] + SIN[,5] + SIN[,6] + COS[,6])
#nojbolji model

#d)prikazi korelogram i parcijalni korelogram reziduala modela koji se najbolje uklapa i iskomentarisi ih
#sad treba odluciti koji od ova 2 modela sim.lm i harm.lm je bolji i za njega prikazati korelogram i parcijalni korelogram
#koristicemo AIC kriterijum
AIC(sim.lm)
AIC(harm.lm)
#na osnovu njega zakljucak je da je bolji model harm.lm jer je manji AIC
#ispitivanje reziduala za harm.lm
plot.ts(resid(harm.lm))
abline(h=0,col="red") #deluje da je trend dobro objasnjen polinomom 2.stepena jer se reziduali ponasaju slucajno
acf(resid(harm.lm))#postoji visoka pozitivna korelisanost, verovatno kao posledica rastuceg trenda 
pacf(resid(harm.lm)) #postoje znacajne parcijalne korelacije sa koracima 1, 2 a i sa vecim koracima pa nije bas najjasnije da li mozemo da fitujemo nekim AR modelom

#e)za taj najbolji model fituj reziduale jednim AR modelom. Kog je reda i koji su parametri?
#za fitovanje AR modela koristi se funkcija ar
res.ar<-ar(resid(harm.lm))
res.ar$order #red je 23, znaci AR(23)
res.ar$ar #koeficijenti modela
res.ar$var.pred

#f) korelogram reziduala AR modela i komentari
acf(na.omit(res.ar$res)) #korelogram reziduala fitovanog AR(23) modela, deluje da oni cine beli sum, nema znacjanih korelacija
#te reziduale mozemo uzeti da budu beli sum naseg modela
#svakako nas prvobnitni model nije uspeo  u potpunosti da objasni sezonsku komponentu
#to i nije toliko neobicno  s obzirom na to da i sezonska komponenta moze da bude stohasticka, bas kao i trend a harmonici su deterministicke funkcije
pacf(na.omit(res.ar$res)) #nema znacajnih parcijalnih korelacija
sd(na.omit(res.ar$res)) 

#g)zapisi jednacinu najboljeg modela
summary(harm.lm)
vreme<-time(Elec.ts)
mean(vreme);sd(vreme)
#xt u redu ispod je vrednost logaritmovane pocetne serije u vremenskom trenutku t, sa svim ocenjenim parametrima
#xt=8.672+0.598(t-1974.458)/9.538-0.0904534(t-1974.458)^2/9.538^2-0.006SIN[t, 1]-0.106COS[t, 1]-0.005SIN[t, 2]+0.0198COS[t, 2]-0.015COS[t, 3]- 0.024SIN[t, 5]-5.725e^8SIN[t, 6]+0.007COS[t, 6]+zt
#SIN[t,i]=sin(2pi*t*i) i COS[t,i]=cos(2pi*t*i) 
#t vreme u godinama 
#zt serija reziduala objasnjena AR(23) procesom sa koeficijentima:
res.ar$ar

#h)iskoristi taj najbolji model za prognoziranje potrosnje elektricne energije u periodu 1991-2000.
#ukoliko izvrsimo logaritmovanje a zatim eksp.transformaciju na taj nacin dobijenih predvidjenih vrednosti dolazi do pristrasnosti u prognozi
#ukoliko model dobro odgovara podacima onda je ta pristrasnost mala
#xt=log(yt)=>yt=e^xt gde je yt pocetna serija Elec.ts a xt serija nastala logaritmovanjem
#xt=mt+st+..+wt=>yt=...e^wt
#uz pretpostavku da je ostatak koji se dobija nakon formiranja modela Gausov beli sum sa N(0,sigmakv) raspodelu e^wt imace lognormalnu sa srednjom vrednoscu e^(sigmakv/2)
#korektivni faktor kojim mnozimo rezultat je e^(sigmakv/2)
new.t<-seq(1991,len=10*12,by=1/12)#predvidjanje ce biti zakljucno sa decembrom 2000.
mean(vreme); sd(vreme)
time=(new.t-1974.458)/9.5383 
SIN<-COS<-matrix(nrow=length(new.t),ncol = 6)
for(i in 1:6)
{
  SIN[,i]=sin(2*pi*i*new.t)
  COS[,i]=cos(2*pi*i*new.t)
}
new.dat<-data.frame(vreme=time,sin1=SIN[,1],cos1=COS[,1],sin2=SIN[,2],cos2=COS[,2],cos3=COS[,3],sin5=SIN[,5],sin6=SIN[,6],cos6=COS[,6])
pr<-predict(harm.lm,new.dat) #predvidjanje za log(Elec.ts), tj za trend i sezonsku komponentu
prar <- predict(ar(harm.lm$res),n.ahead = 10*12) #predvidjanje za AR(23) model
#da bismo se vratili na pocetnu treba nam eksponencijalna raspodela
Elecpreddicted<-exp(pr+prar$pred+0.0003982/2)
Elecpreddicted.ts<-ts(Elecpreddicted,start = 1991,frequency = 12)
ts.plot(Elec.ts,Elecpreddicted.ts,lty=1:2)
#predvidjanjem se nista znacajno nije promenilo za narednih 10 godina

#5.5 
#Hidrolog zeli da prognozira prosecnan mesecni priliv vode(m^3/s) u Font Reservoir za narednih 10 godina
#dati su podaci od 1.1909 do 12.1980
www <- "http://www.math.rs/p/files/69-inflow.txt"
inflow <- read.table(www, header=TRUE)
inflow.ts<-ts(as.vector(t(inflow)), start=1909, freq=12)
plot(inflow.ts, ylab = 'inflow')

#a) Koristeci linearnu regresiju prikazi zavisnost dotoka vode od indikatorskih promenljivih kao i promenljive vreme i fituj pogodan AR model za seriju reziduala
vreme<-time(inflow.ts);sezone<-cycle(inflow.ts);vreme<-(vreme-mean(vreme))/sd(vreme)
fr.lm<-lm(inflow.ts~0+vreme+factor(sezone))
summary(fr.lm)
step(fr.lm) #ovaj model je bolji od manjih modela, koristili stepwise regresiju AIC=-1802
#posmatramo reziduale modela
acf(resid(fr.lm))# korelogram ima izgled cos fje pa se zakljuci da je neki AR model u pitanju
pacf(resid(fr.lm)) #moglo bi se zakljucimo da je u  pitanju AR(1) model jer je samo parcijalna korelacija sa korakom 1 znacajna
#fitujemo te reziduale AR modelom, koristeci funkciju ar
res.ar<-ar(resid(fr.lm))
res.ar$order #u pitanju je AR(2) model
res.ar$ar
acf(res.ar$res[-c(1,2)]) #vidimo da AR model dobro objasnjava reziduale pocetnog modela (iskljucujemo prva 2)

#b)prikazi histogram reziduala AR modela i iskomentarisi
hist(res.ar$res[-c(1,2)])  #zakljucimo da reziduali nisu simetricni oko 0
#fituj back-to-back Vejbulovu raspodelu greskama
resids<-res.ar$res[-c(1,2)]
library(MASS)
pos.resids <- resids[resids > 0]  # subset the positive residuals
neg.resids <- resids[resids < 0]  # subset the negative residuals
neg.resids <- neg.resids * -1  # transform the negative residuals
pos.Weibull <- fitdistr(pos.resids, "weibull")  # fit a Weibull to the positive residuals
neg.Weibull <- fitdistr(neg.resids, "weibull")  # fit a Weibull to the (transformed) negative reisduals

#d)razlozi zasto  log transformacija  moze biti pogodna i za seriju inflow
plot(inflow.ts, ylab = 'inflow')
#ovde nije bas najjasnije kao u prethodnom primeru da bi log transformacija bila efikasna,nije jasan trend, postoji sezonska komponenta
#svakako se vidi da je disperzija poprilicno nestabilna i da postoje jako velike vrednosti disperzije
plot(log(inflow.ts),ylab = 'log(inflow)')
#log transformacija nije nista znacajno promenila

#e)Regress log(inflow) koristeci indikatorske promenljive kao i promenljivu vreme 
#i fituj pogodan AR model za seriju reziduala
vreme<-time(inflow.ts);sezone<-cycle(inflow.ts);vreme<-(vreme-mean(vreme))/sd(vreme)
fr.lm<-lm(log(inflow.ts)~0+vreme+factor(sezone))
summary(fr.lm)
step(fr.lm) #koristimo opet regresiju stepwise da bi nasli najbolji model, radi po principu AIC
acf(resid(fr.lm))# korelogram ima izgled cos fje pa se zakljuci da je neki AR model u pitanju
pacf(resid(fr.lm)) #moglo bi se zakljucimo da je u  pitanju AR(1) model jer je samo parcijalna korelacija sa korakom 1 znacajna
#fitujemo te reziduale AR modelu
res.ar<-ar(resid(fr.lm))
res.ar$order
res.ar$ar
#opet je u pitanju AR(2) model 
acf(res.ar$res[-c(1,2)]) #vidimo da AR model i u ovom slucaju dobro objasnjava reziduale pocetnog modela

#f)prikazi histogram reziduala i iskomentarisi
hist(res.ar$res[-c(1,2)]) #deuje da imaju normalni raspodelu
qqnorm(res.ar$res[-c(1,2)])
#fituj back-to-back Vejbulovu raspodelu gresaka
resids<-res.ar$res[-c(1,2)]
pos.resids <- resids[resids > 0]  # subset the positive residuals
neg.resids <- resids[resids < 0]  # subset the negative residuals
neg.resids <- neg.resids * -1  # transform the negative residuals
pos.Weibull <- fitdistr(pos.resids, "weibull")  # fit a Weibull to the positive residuals
neg.Weibull <- fitdistr(neg.resids, "weibull")  # fit a Weibull to the (transformed) negative reisduals



#6.4
#a)fituj pogodan model za air passangers seriju
install.packages("datasets")
library(datasets)

data(AirPassengers)
AP <- AirPassengers
AP
plot(AP) #jasno izrazen trend, sezonska komponenta
plot(log(AP))

# Objasnjenje iz knjige
# The variance can be seen to increase as t increases, whilst
# after the logarithm is taken the variance is approximately constant over the
# period of the record. Therefore, as the number of people using
# the airline can also only be positive, the logarithm would be appropriate in
# the model formulation for this time series. In the following code, a harmonic
# model with polynomial trend is fitted to the air passenger series. The function
# time is used to extract the time and create a standardised time variable TIME.

SIN <- COS <- matrix(nr = length(AP), nc = 6)
for (i in 1:6) {
  SIN[, i] <- sin(2 * pi * i * time(AP))
  COS[, i] <- cos(2 * pi * i * time(AP))
}
TIME <- (time(AP) - mean(time(AP)))/sd(time(AP))
sd(time(AP))
# model sa svim prediktorima
AP.lm1 <- lm(log(AP) ~ TIME + I(TIME^2) + I(TIME^3) + I(TIME^4) + SIN[,1] + COS[,1] + SIN[,2] + COS[,2] + SIN[,3] + COS[,3] +SIN[,4] + COS[,4] + SIN[,5] + COS[,5] + SIN[,6] + COS[,6])
summary(AP.lm1)
step(AP.lm1)
# na osnovu najmanjeg ACI, najbolji model je seldeci
AP.lm2 <- lm(log(AP) ~ TIME + I(TIME^2) + I(TIME^4) + SIN[, 1] + COS[, 1] + SIN[, 2] + COS[, 2] + SIN[, 3] +  COS[, 3] + SIN[,4] + COS[, 4] + SIN[, 5])
summary(AP.lm2)
# AP.lm3 <- lm(log(AP) ~ TIME + I(TIME^2) + I(TIME^4) + SIN[, 1] + COS[, 1] + SIN[, 2] + COS[, 2] + SIN[, 3]  + SIN[,4] + COS[, 4] + SIN[, 5])
# summary(AP.lm3) - svi prediktori su znacajni


par(mfrow=c(1,2))
acf(resid(AP.lm2)) 
pacf(resid(AP.lm2)) # ukazuje na to da je vrv beli sum
par(mfrow=c(1,1))


#b) Uklopi reziduale iz dela a) u ARMA(p,q), gde su p i q<=2
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:2) for (j in 0:2) {
  fit.aic <- AIC(arima(resid(AP.lm2), order = c(i, 0,
                                                j)))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(resid(AP.lm2), order = best.order)
    best.aic <- fit.aic
  }
  
}

best.order # dobili smo da je najbolji ARMA(1,2) model
coef(best.arma)[-4] # ocenjeni autoregresioni parametri
AIC(AP.lm2)
AIC(best.arma) # ovo je za sad najbolji model, jer ima najmanji AIC
acf(resid(best.arma)) 

#c)
new.t <- time(ts(start = 1961, end = c(1961, 12), fr = 12))
TIME <- (new.t - mean(time(AP)))/sd(time(AP))
SIN <- COS <- matrix(nr = length(new.t), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * new.t)
  SIN[, i] <- sin(2 * pi * i * new.t)
}
SIN <- SIN[, -6]
new.dat <- data.frame(TIME = as.vector(TIME), SIN = SIN, COS = COS)
AP.pred.ts <- exp(ts(predict(AP.lm2, new.dat), start = 1961, fr=12))
ts.plot(log(AP), log(AP.pred.ts), lty = 1:2)
# model se dobro uklapa

#6.5
#a) napisi funkciju koja racuna acf za ARMA(1,1) proces. Treba da ima parametre koji predstavljaju koeficijente AR i MA dela
acfun <- function(k,alfa,beta)
{
  rho.k <- rep(0,k)
  rho.k[1] <- 1
  for (i in 2:k)
    rho.k[i] = alfa^(i-1)*(alfa+beta)*(1+alfa*beta)/(1+alfa*beta+beta^2)
  return(rho.k)
}

#b)predstavi graficki funkciju za alfa=0.7 i beta=-0.5 za korake od 0 do 20
plot(acfun(20,0.7,-0.5),xlab = "Lag",ylab = "ACF", type = "h")
abline(h=0)

#c)simuliraj 100 vrednosti ARMA(1,1) procesa sa gore zadatim koef i napravi korelogram  
sim100<-arima.sim(100,model=list(order(1,0,1),ar=c(0.7),ma=c(-0.5)))
acf(sim100)
#za 1000
sim1000<-arima.sim(1000,model=list(order(1,0,1),ar=c(0.7),ma=c(-0.5)))
acf(sim1000)


#7.4 smatramo da je wt beli sum sa srednjom vrednoscu 0
#broj prekookeanskih posetilaca koji su dolazili u Novi Zeland, zabelezen mesecni broj pocevsi od 1977. do 1995
www<-"http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/osvisit.dat"
visit<-read.table(www, header=FALSE)
visit.ts<-ts(as.vector(visit),start = 1977,frequency = 12)
plot(visit.ts,xlab="vreme",ylab="broj posetilaca u svakom mesecu") #rastuci trend, sezonska komponenta kao i varijansa koja raste sa vremenom
logvisit.ts<-ts(log(as.vector(visit)),start = 1977,frequency = 12)
plot(logvisit.ts,xlab="vreme",ylab="log(broj posetilaca u svakom mesecu)")
#a)korelogram za seriju logvisit.ts
acf(logvisit.ts)
#serija je autokorelisana
#korelogram je periodican sa korakom 12, sto ukazuje na postojanje sezonske koponente 

#b)uklopi u ARIMA(1,1,0)
mdlarima<-arima(logvisit.ts,order=c(1,1,0))
acf(resid(mdlarima)) #deluje kao da postoji sezonska komponenta koju treba ukloniti, ovaj model nije dobar

#c)uklopi u sezonski ARIMA(1,1,0)(0,1,0)_12
modelsarima <-arima(logvisit.ts, order=c(1,1,0),seas=list(order=c(0,1,0),12))
acf(resid(modelsarima)) #ni ovde nije u potpunosti uklonjen sezonski efekat

#d)izaberi najbolji sezonski ARIMA model na osnovu AIC kriterijuma
AIC(arima(logvisit.ts,order=c(1,1,0),seas=list(order=c(1,1,0),12)))#-528.214
AIC(arima(logvisit.ts,order=c(0,1,1),seas=list(order=c(0,1,1),12)))#-528.214
AIC(arima(logvisit.ts,order=c(1,1,0),seas=list(order=c(0,1,1),12)))#-535.6859
AIC(arima(logvisit.ts,order=c(0,1,1),seas=list(order=c(1,1,0),12)))#-558.1545
AIC(arima(logvisit.ts,order=c(1,1,1),seas=list(order=c(1,1,1),12)))#-564.2525
AIC(arima(logvisit.ts,order=c(1,1,1),seas=list(order=c(1,1,0),12)))#-560.0888
AIC(arima(logvisit.ts,order=c(1,1,1),seas=list(order=c(0,1,1),12)))#-565.2818
bestsarima<-arima(logvisit.ts,order=c(1,1,1),seas=list(order=c(0,1,1),12)) #najbolji sa najmanjom vrednoscu AIC
bestsarima
#korelogram reziduala
acf(resid(bestsarima)) #ovo je beli sum, model je dovoljno dobar

#e) 
# ARIMA(p,d,q)(P,D,Q)s

# TETA_P*(B^s)teta_p(B)(1 ??? B^s)^D(1 ??? B)^d*x_t = FI_Q(B^s)fi_q(B)w_t


# AR:  teta(B) = 1 - teta_1*B - ... - teta_p*B^p
# MA:  fi(B) = 1 + fi_1*B + ... + fi_q*B^q

# The seasonal components are:
# Seasonal AR:  TETA(B^s) = 1 - TETA_1*B^s - ... - TETA_P*B^(Ps)
# Seasonal MA:  FI(B^s) = 1 + FI_1*B^s + ... + FI_Q*B^(Qs)
# s=12, D=d=1

# (1-0.2492B^12)(1-0.2562B)(1-B^12)(1-B)z_t = (1+0.1352B^12)(1+0.0589B)w_t

#f)testiranje reziduala na stacionarnost
#testovi jedinicnog korena:
adf.test(resid(bestsarima)) #Augmented Dickey-Fuller Test, null: x has a unit root. ovde odbacujemo hipotezu jer je p vrednost testa 0.01, nema jedinicnih korena, stacionarna je
kpss.test(resid(bestsarima))# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for the null hypothesis that x is level or trend stationary.
#ovde prihvatamo nultu
#dakle serija reziduala jeste stacionarna
acf(resid(bestsarima))
plot(resid(bestsarima)) #reziduali imaju priblizno istu srednju vrednost i disperziju,pa i ovde zakljucujemo da 
#je serija reziduala stacionarna

#g)predvidjanje za sve mesece  1996 godine
predvidjanje<-exp(predict(bestsarima,n.ahead = 12)$pred+bestsarima$sigma2/2)
predvidjanje #po mesecima
sum(predvidjanje)#u 1996.

#7.6
#podaci o berzi u 7 gradova, od 6.1.1986. do 31.12.1997
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/stockmarket.dat"
x = read.table(www,header = TRUE)
attach(x)
Amsterdam <- ts(Amsterdam, start = 1986)

#a)prikazi seriju AMsterdam i diff(Amsterdam)
par(mfrow = c(1,2))
plot(Amsterdam) # rastuci trend kao i varijacija koja nije konstantna
plot(diff(Amsterdam)) # diferenciranje serije je uklonilo trend, ali je ostala nekonstantna disperzija
par(mfrow = c(1,1))

#b)treba fitovati neke ARIMA modele Amsterdam seriji i videti koji je najprihvatljiviji
AIC(arima(Amsterdam,order = c(0,1,0))) 
AIC(arima(Amsterdam,order = c(1,1,0)))
AIC(arima(Amsterdam,order = c(0,1,1)))
AIC(arima(Amsterdam,order = c(1,1,1)))
#ARIMA(0,1,0) je najbolji, ima najmanji AIC
best<-arima(Amsterdam,order = c(0,1,0))

#c)Korelogram reziduala najboljeg modela kao i korelogram kvadrata reziduala
acf(best$residuals)  #reziduali nisu stacionarna serija
acf((best$residuals)^2)#sve korelacije su statisticki znacajne

#d)treba fitovati neke GARCH modele rezidualima prethodnih modela i videti koji je najprihvatljiviji
#kriterijum za izbor najboljeg modela bice AIC
library(tseries)
AIC(garch(best$residuals,order = c(0,1)))#17613.54
AIC(garch(best$residuals,order = c(1,0)))
AIC(garch(best$residuals,order = c(1,1)))# 16142.38
AIC(garch(best$residuals,order = c(0,2)))#16939.75
bestgarch<-garch(best$residuals,order = c(1,1))
confint(bestgarch) #intervali poverenja za parametre
bestgarch #koeficijenti

#e)korelogram reziduala najboljeg modela kao i korelogram kvadrata reziduala
par(mfrow = c(1,2))
acf(bestgarch$residuals[-1])#samo 2 vrednosti prelaze preko plave linije
acf((bestgarch$residuals[-1])^2)
#vidi se da je GARCH(1,1) model dobar jer se za njegove reziduale moze reci da su beli sum




