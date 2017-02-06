### domaci iz Vremenskih serija

# zadatak 1

www<-"http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/global.dat"
Global<-scan(www) #ucita u niz
class(Global) #nije ts, vec numaric pa zato pravimo ts
Global.ts<-ts(Global,frequency=12,start=c(1856,1),end=c(2005,12))
start(Global.ts) #pocetak serije
end(Global.ts) #kraj serije
Global.ts  #originalna serija (u mesecima)
plot(Global.ts) #grafik serije, mozemo zakljuciti da nije stacionarna nema nekih pravilnosti

#a)

Global.annual<-aggregate(Global.ts)/12  #godisnja serija
Global.annual
par(mfrow=c(1,2))
#graficki prikaz mesecne i godisnje vs
plot(Global.ts) 
plot(Global.annual)
#grafik mesecne i godisnje vs

boxplot(Global.ts~cycle(Global.ts)) #prosecne temperature po mesecima prikazane na boxplot-u

#b)

Global.decompose<-decompose(Global.ts)
plot(Global.decompose)  #aditivni model
Global.decom.mult <- decompose(Global.ts, type = "mult") #multiplikativni model
plot(Global.decom.mult)
#kako sezonska komponenta ne raste sa porastom trenda, koristimo aditivni model
#funkciji decompose prosledjujemo objekat tipa vremenska serija a dobijamo
#objekat tipa decomposed.ts cije komponente mozemo videti pozivom funkcije summary
summary(Global.decompose)
Global.trend<-Global.decompose$trend #izdvajamo trend
Global.trend
Global.seasonal<-Global.decompose$seasonal #izdvajamo sezonsku komponentu
Global.seasonal
ts.plot(cbind(Global.trend,Global.trend+Global.seasonal)) #graficki prikaz trenda i trenda i sezonske komponente
plot(Global.trend) #samo trend

#c)

plot(Global.ts) 
acf(Global.ts) #vidimo da korelacije sporo opadaju sto je posledica rastuceg trenda
acf(Global.ts)$acf #mozemo dobiti i odgovarajuci niz brojeva
acf(Global.ts)$acf[1] #vrednost autokorelacione funkcije u nuli je jedan, sto je upravo vrednost prvog elementa niza
acf(Global.ts, type = "covariance")$acf[2] #vrednost autokovarijacione funkcije dobijamo kada dodamo jos jedan element niza
Global.decompose$random #prvih i poslednjih 6 vrednosti su NA vrednosti pa njih treba ukloniti
n<-(length(Global.decom.add$random)-6)
acf(Global.decom.add$random[7:n]) #korelogram reziduala
#vidimo da je najveca pozitivna korelacija sa korakom 1 (statisticki je znacajna), a najveca negativna korelacija je sa korakom 5
#takodje, mozemo reci da je statisticki znacajna i autokorelacija sa korakom 3,4,6,25..
#vidimo da ima oblik funkcije kosinusa sto znaci ili da je model los ili da je model procesa greska AR(2) model
sd(Global.ts[7:138])
sd(Global.ts[7:138] - Global.decom.add$trend[7:138])
sd(Global.decom.add$random[7:138])
#uklanjanje deterministickih komponenti dovodi do smanjenja disperzije
#a posto je disperzija mala, mozemo zakljuciti da je model dobar i da je debijeni oblik korelograma posledica AR(2) modela

#d)

Global.hw<-HoltWinters(Global.ts)  #aditivni model
Global.hw #ispisuju se parametri modela i koeficijenti (vrednosti koje sluze za predvidjanje)
Global.hw$SSE #minimizirana greska predvidjanja jedan korak unapred
plot(Global.hw) #crna linija nam je pocetna serija, a crvena je serija izravnjana HW metodom
plot(Global.hw$fitted)
#Kako je vrednost alfa jednaka 0.34, tj. vrednost je mala, vidimo da je ona bazirana na prethodnoj i i nekim opservacijama iz proslosti
#kako je vrednost bete mala kod aditivnog modela, to znaci da se za vrednost nagiba uzima poslednja ocenjena vrednost, sto znaci, ocenjeno je da se nagib trenda
#sporo menja za razliku od nivoa sezonske varijacije
#vrednost game je takodje mala, to znaci da je vrednost sezonske komponente bazirana na nekim daljim opservacijama
#kako smo videli da serija ima i trend i sezonsku komponentu, parametre beta i gama ne treba uklanjati
Global.hw.mult<-HoltWinters(Global.ts, seasonal = "mult")
Global.hw.mult
plot(Global.hw.mult)
#kao sto smo videli i malopre, bolji je aditivni model jer crvena linija vise prati crnu!

#predvidjanje
Global.hw.predict<-predict(Global.hw, n.ahead=5*12) #predvidjanje za 5 godina unapred
Global.hw.predict
ts.plot(Global.ts,Global.hw.predict,lty=1:2)

#Drugi nacin za prognoziranje- paket forecast (dobija se isti rezultat kao i malopre)
install.packages("forecast")
library(forecast)
Global.hw.predict2<-forecast.HoltWinters(Global.hw, h=5*12)
Global.hw.predict2

#proveravamo da li je model dobar, tj da li nema serijske korelacije u seriji reziduala, 
#da li su reziduali normalno raspodeljeni i da li je disperzija serije reziduala konstantna tokom vremena

acf(Global.hw.predict2$residuals) #grafik korelograma pokazuje da ima par znacajnih korelacija
#Ljung-Box test takodje testira da li ima neke statisticki znacajne korelacije
Box.test(Global.hw.predict2$residuals, type="Ljung-Box")
#Nulta hipoteza je da su sve korelacije jednake 0, a kako je ovde p-vrednost testa mala odbacujemo nultu hipotezu
#odnosno, postoje znacajne korelacije

#proveravamo da li su reziduali normalno raspodeljeni

qqnorm(Global.hw.predict2$residuals) #po grafiku bismo mozda rekli da jesu normalni
#ali bolje je uveriti se u to jos nekim testom

install.packages("tseries")
library(tseries)
jarque.bera.test(Global.hw.predict2$residuals) #ne mozemo prihvatiti pretpostavku o normalnosti reziduala


#proveravamo da li je disperzija serije reziduala konstantna tokom vremena

plot(Global.hw.predict2$residuals) #sa grafika bismo rekli da je mozda ipak veca disperzija na pocetku, a kasnije manja

# zadatak 2

#xt=mi+wt+0.8*w{t-1}, mi nepoznat parametar
#nepristrasna ocena za mi bice uzoracka sredina, mean(x) jer je za beli sum ocekivanje 0
greska1<-function(mi){
  w<-rnorm(100)
  x<-mi+w[1]
  for (t in 2:100) x[t] <- mi+w[t]+0.8*w[t-1]
  ts.plot(x)
  abline(h=mean(x))
  return(mi-mean(x))
}
greska1(10)

#xt=mi+wt, mi nepoznat parametar
#nepristrasna ocena za mi bice uzoracka sredina, mean(x) jer je za beli sum ocekivanje 0
greska2<-function(mi){
  x2<-w2<-rnorm(100)
  for (t in 1:100) x2[t] <-mi+w2[t]
  ts.plot(x2)
  abline(h=mean(x2))
  return(mi-mean(x2))
}
greska2(10)


abs(greska1(2))-abs(greska2(2))
abs(greska1(1))-abs(greska2(1))
abs(greska1(0.5))-abs(greska2(0.5))

#za realizovanu seriju x1,.....,xn  gde je x1=mi*w1,x2=mi+w2+0.8*w1,...,xn=mi+wn+0.8*w{n-1}
#mean(x_1)=(n*mi+1.8w1+1.8w2+...+1.8w{n-1}+wn)/n = mi + (1.8*(w1+w2+...+w{n-1})+wn)/n
#za x_i=mi+w_i
#mean(x_2)=(suma(i ide od 1 do n) {mi+w_i}) / n = (n*mi+suma(i ide od 1 do n ) {w_i}) * (1/n) = mi+(suma(i ide od 1 do n ) {w_i})*(1/n) 
#D(x_1)=1/n^2 * D(1.8*(w1+w2+...+w{n-1})+wn) = 1/n^2 (1,8^2* D(w1+w2+...+w{n-1})+D(wn))= 1/n^2*(1.8^2*(n-1)sigma^2+sigma^2)=
#...=1.8^2 * sigma^2 * 1/n + (1-1.8^2)*sigma^2 * 1/n^2 = 1.8^2 * sigma^2 * 1/n - 2.24 sigma^2 * 1/n^2
#D(x_2)=1/n^2 * suma(D(wi))=1/n^2*n*sigma^2=1/n * sigma^2
#D(x_1)-D(x_2)=...=2.24*sigma^2 * (1/n - 1/n^2) >= 0
#dakle vece je standardno odstupanje ocene u 1.slucaju



# zadatak 3

simulacija<-function(A, omega, fi){
  set.seed(1)
  w<-rnorm(500)
  x<-vector()
  for (t in 1:500) x[t] <-A*cos(2*pi*omega*t+fi)+w[t]
  plot(x, type = "l")
  return(x)
}


s1<-simulacija(0,0,0) # u ovom slucaju nece biti deterministickog dela jer A=0,ostaje samo wt koji je predstavljen N(0,1) raspodelom
s2<-simulacija(4,4,0) #u ovom slucaju, kada je omega ceo broj, a fi=0, i cos je 2pi periodicna i to je 1, i bice samo A+wt
s3<-simulacija(13,1/2,0) #u ovom slucaju, imacemo 13cos(pi*t)+wt, a cos(pi*t) ce biti -1,1,-1,... pa serija u tom slucaju periodicno menja znak,sa periodom 2
s4<-simulacija(10,3,1) #10*cos(2*pi*3*t+1)+wt
s5<-simulacija(10,0.33,1)#deterministicki deo bice fja Acos(2*omega*pi+fi)=A(cos(2*pi*omega)*cosfi-Asin(2*pi*omega)sin(fi)

#crtamo korelograme za ove serije
acf(s1) #kako je u ovom slucaju serija samo beli sum, dobili smo i odgovarajuci korelogram koji pokazuje da ne odbacujemo H0 odnosno nekorelisane su
acf(s2) #u ovom slucaju dobijemo slican korelogram koji odgovara belom sumu i pokazuje da su nekorelisane
acf(s3) #postoji velika korelisanost izmedju svake 2, s tim da je pozitivna za parne korake i negativna za neparne
acf(s4) # slicno kao za acf(s2)
acf(s5) # postoji korelisanost

#mozemo da zakljucimo da korelisanost najvise zavisi od parametra omega,tj. kada je on ceo broj nema korelacije, dok u suprotnom se javlja jaka korelisanost
#A je amplituda, fi je faza a omega frekvencija

#xt=cos(2*pi*t/5)+wt je specijalan slucaj prethodnog modela  
serija<-simulacija(1,1/5,0) #u konkretnoj seriji A=1, omega=1/5 i fi =0
class(serija) #klase je numeric
serija.ts<-ts(serija, frequency = 5)
serija.ts
plot(serija.ts)

serija.decompose<-decompose(serija.ts)  #funkcija decompose koristi metodu pokretnih proseka za ocenu trenda
serija.decompose
plot(serija.decompose)
trend<-serija.decompose$trend #izdvajamo trend
trend
plot(trend)

plot(serija.ts)
points(seq(0,100, length = 500), trend, type = "l", col="blue") #dodajemo izravnatu seriju na grafik
ts.plot(cbind(serija.ts, trend), type="l", lty=c(1,2))


# zadatak 4

set.seed(1)
TIME<-1:(15*12)
w<-rnorm(15*12, sd = 0.5)

s<-0.8*cos(2*pi*TIME/12+120) 
x<-s+w #pravimo seriju trazenog oblika, A, omega i fi smo uzeli proizvoljno
plot(x,type="l",xlab="Time")

acf(x) #vidimo da je period stvarno 12

# X_t=A*cos(2*pi*omega*t+fi)+W_t 
# A*cos(2*pi*omega*t+fi)=A*cos(2*pi*omega*t)*cos(fi)-A*sin(2*pi*omega*t)*sin(fi)
# =alfa*cos(2*pi*omega*t)+beta*sin(2*pi*omega*t), gde je alfa=A*cos(fi), beta=-A*sin(fi)

SIN <- COS <- matrix(nr = length(TIME), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * TIME/12)
  SIN[, i] <- sin(2 * pi * i * TIME/12)
}
#pravimo model za nasu seriju
model<-lm(x~COS[, 1]+SIN[, 1]+COS[, 2]+SIN[, 2]+COS[, 3]+SIN[, 3]+COS[, 4]+
            +SIN[, 4]+COS[, 5]+SIN[, 5]+COS[, 6]+SIN[, 6])
model
summary(model) #odavde vidimo da imamo znacajnih prediktora, tj. cos[,1] , sin[,1] i sin[,5]
#zato cemo napraviti jos bolji model

modelPoboljsani<-lm(x~0+COS[, 1]+SIN[, 1]+SIN[, 5]) #postavimo da nema intercept-a (jer u nasoj seriji nema)
modelPoboljsani
summary(modelPoboljsani)
#sada su nam svi prediktori znacajni

AIC(model) #velika vrednost
AIC(modelPoboljsani)  #manji AIC pa je model bolji


y<-modelPoboljsani$coefficients[1]*cos(2*pi*TIME/12)+modelPoboljsani$coefficients[2]*sin(2*pi*TIME/12)+
  modelPoboljsani$coefficients[3]*sin(2*pi*5*TIME/12)
plot(x,type="l",xlab="Time") #crtamo nasu seriju
points(TIME, y, type = "l", col="blue") #dodajemo poboljsani model da bismo videli da li se dobaro uklopio
#nije toliko lose :)

www<-"http://www.math.rs/p/files/69-regr1.txt"
regr1<-scan(www)
regr1
class(regr1)
regr1.ts<-ts(regr1)
regr1.ts

plot(regr1.ts)


acf(regr1.ts)  #periodicnost posle 8 koraka
TIME<-time(regr1.ts)

SIN <- COS <- matrix(nr = length(TIME), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * TIME/8)
  SIN[, i] <- sin(2 * pi * i * TIME/8)
}
#pravimo model
model1<-lm(regr1.ts~COS[, 1]+SIN[, 1]+COS[, 2]+SIN[, 2]+COS[, 3]+SIN[, 3]+COS[, 4]+
             +SIN[, 4]+COS[, 5]+SIN[, 5]+COS[, 6]+SIN[, 6])
model1
summary(model1) #statisticki znacajni prediktori su cos[,1],sin[,1],cos[,2]

#pravimo model samo sa znacajnim prediktorima
model1.poboljsanje<-lm(regr1.ts~COS[, 1]+SIN[, 1]+COS[, 2])
summary(model1.poboljsanje)

#model bez intercept
model1.poboljsanje2<-lm(regr1.ts~0+COS[, 1]+SIN[, 1]+COS[, 2])
summary(model1.poboljsanje2)

y<-model1.poboljsanje2$coefficients[1]*cos(2*pi*TIME/8)+model1.poboljsanje2$coefficients[2]*sin(2*pi*TIME/8)+
  model1.poboljsanje2$coefficients[3]*sin(2*pi*2*TIME/8)
plot(regr1.ts,type="l",xlab="Time")
points(TIME, y, type = "l", col="blue")

#provera da li je model odgovarajuci na osnovu korelograma
acf(model1.poboljsanje2$residuals)
#imamo jednu stat znacajnu korelaciju sto je ok
Box.test(model1.poboljsanje2$residuals, type="Ljung-Box")
#velika p-vrednost testa u Box-Ljungovom testu ukazuje na nekorelisanost reziduala

#Test Zark Bera za testiranje normalnosti reziduala
jarque.bera.test(model1.poboljsanje2$residuals)
#velika p vrednost testa tako da mozemo prihvatiti hipotezu o normalnoj raspodeljenosti reziduala
#provericemo jos i histogramom
hist(model1.poboljsanje2$residuals) #ima oblik normalne raspodele
plot(model1.poboljsanje2$residuals) #mozemo reci da je disperzija konstantna


www2<-"http://www.math.rs/p/files/69-regr2.txt"
regr2<-scan(www2)
regr2
regr2.ts<-ts(regr2)
regr2.ts

plot(regr2.ts)

regr2.ts2<-ts(regr2, frequency = 8)
regr2.decompose<- decompose(regr2.ts2)
plot(regr2.decompose)
acf(regr2.ts) #frekvencija je 8

model2<-lm(regr2.ts~COS[, 1]+SIN[, 1]+COS[, 2]+SIN[, 2]+COS[, 3]+SIN[, 3]+COS[, 4]+
             +SIN[, 4]+COS[, 5]+SIN[, 5]+COS[, 6]+SIN[, 6])
model2
summary(model2) #znacajni prediktori cos[,1], sin[,1]

#pravimo model samo sa znacajnim prediktorima
model2.poboljsanje<-lm(regr2.ts~COS[, 1]+SIN[, 1])
summary(model2.poboljsanje)

#model bez intercept
model2.poboljsanje2<-lm(regr2.ts~0+COS[, 1]+SIN[, 1])
summary(model2.poboljsanje2)

y<-model2.poboljsanje2$coefficients[1]*cos(2*pi*TIME/8)+model2.poboljsanje2$coefficients[2]*sin(2*pi*TIME/8)
plot(regr2.ts,type="l",xlab="Time")
points(TIME, y, type = "l", col="blue")
#probacemo da napravimo jos bolji model tako sto cemo dodati TIME da ocenimo trend
model3<-lm(regr2.ts~TIME+I(TIME^2)+COS[, 1]+SIN[, 1]+COS[, 2]+SIN[, 2]+COS[, 3]+SIN[, 3]+COS[, 4]+
             +SIN[, 4]+COS[, 5]+SIN[, 5]+COS[, 6]+SIN[, 6])
summary(model3)

model3.poboljsanje<-lm(regr2.ts~TIME+I(TIME^2)+COS[, 1]+SIN[, 1])
summary(model3.poboljsanje)

y<-model3.poboljsanje$coefficients[2]*TIME+model3.poboljsanje$coefficients[3]*TIME^2+model3.poboljsanje$coefficients[4]*cos(2*pi*TIME/8)+model3.poboljsanje$coefficients[5]*sin(2*pi*TIME/8)
plot(regr2.ts,type="l",xlab="Time")
points(TIME, y, type = "l", col="blue")


# zadatak 5

install.packages("astsa")
library(astsa)
data(soi)
data(rec)
soi
rec
class(soi) #proveravamo klasu
class(rec)
start(soi) #pocetak serije
end(soi) #kraj serije
start(rec)
end(rec)

#a)
plot(soi)
plot(rec)
ts.plot(soi, rec, lty = c(1,3)) #na jednom grafiku

#u obe serije uocavamo periodicnost, tj sezonsku komponentu
#ciklusi se brze ponavljaju u soi nego u rec
#ove dve serije izgledaju zavisno, u smislu da broj novih roba zavisi od SOI indeksa

acf(soi)  #period je 12
acf(rec)

acf(ts.union(soi,rec)) #obe su periodicne,zavisne,na grafiku vidimo da je vise od 5% (prelazi nivo poverenja)
#c_k statisticki znacajno pa zakljucujemo da su soi i rec zavisne 
#kros korelaciona funkcija sa negativnom zadrskom -k govori da SOI prednjaci u odnosu na REC
ccf(soi,rec)

#kako ove vremenske serije nisu stacionarne pa treba prvo ukloniti trend i sezonsku komponentu
#jer je ccf definisana za stacionarne serije
soi.ran<-decompose(soi)$random #izbacujemo trend i sezonsku komponentu, ostaje samo slucajna
soi.ran #da vidimo gde su NA vrednosti koje treba izbaciti
soi.ran.ts<-window(soi.ran,start=c(1950,7),end=c(1987,3))

rec.ran<-decompose(rec)$random
rec.ran #da vidimo gde su NA vrednosti
rec.ran.ts<-window(rec.ran,start=c(1950,7),end=c(1987,3))
soi.ran.ts
acf(ts.union(soi.ran.ts,rec.ran.ts))
ccf(soi.ran.ts,rec.ran.ts)
cov(soi,rec) #kovarijacija
cor(soi,rec) #koeficijent korelacije


# b) #ovo sam iz knjige i sa interneta nasla, nije mi bas najjasnije pa nisam pisala komentare

alldata=ts.intersect(rec,reclag1=lag(rec,-1), reclag2=lag(rec,-2), soilag5 = lag(soi,-5),
                     soilag6=lag(soi,-6), soilag7=lag(soi,-7), soilag8=lag(soi,-8), soilag9=lag(soi,-9),
                     soilag10=lag(soi,-10))

lag.model1<-lm(rec~soilag5+soilag6+soilag7+soilag8+soilag9+soilag10, data = alldata)
summary (lag.model1)
acf2(residuals(lag.model1))

lag.model2<-lm(rec~reclag1+reclag2+soilag5+soilag6+soilag7+soilag8+soilag9+soilag10,
               data = alldata)
summary (lag.model2)
acf2(residuals(lag.model2))

lag.model3<-lm(rec~reclag1+reclag2+ soilag5+soilag6, data = alldata)
summary (lag.model3)  #najbolji model (najveci R-squared) cak 0.93 i svi prediktori znacajni
acf2(residuals(lag.model3))

new<-data.frame(reclag1=c(43,50,68), reclag2=c(23,20,35), soilag5=c(-0.25,-0.2, -0.5), soilag6=c(-0.30,-0.48, -0.4))
predict(lag.model3, newdata = new)


# zadatak 6

library(astsa)
data("varve")
varve
plot(varve) #ne izgleda kao da je stacionarna

#a) Prvo sto ispitujemo jeste da li serija stacionarna, to mozemo da vidimo ako nadjemo auto-korelacionu funkciju, ACF
acf(varve)
#Naime, kod stacionarnih serija, ACF relativno brzo tezi 0, a kod nestacionarnih ACF sporo opada ka 0, stoga
#ne mozemo prepostaviti stacionarnost nase serije 

#b)
par(mfrow=c(2,1))
plot(varve, main="varve", ylab="")
plot(log(varve), main="log(varve)", ylab="" )

#c)
acf(log(varve)) #vidimo da se ACF za logaritmovanu seriju nije znacajno promenila
#Drugi nacin koji koristimo za transformaciju serije jeste diferenciranje serije log(varve), koje ce 
#eliminisati tend i sezonsku komponentu
diff(log(varve))
plot(diff(log(varve)))
#Sada cemo videti kako izgledaju ACF, odnosno PACF
acf2(diff(log(varve)))

#Posto vidimo da ACF ima jednu znacajnu vrednost u 1, a PACF opada eksponencijalno
#zakljucujemo da je rec o modelu pokretnih proseka reda 1, tj. MA(1).


