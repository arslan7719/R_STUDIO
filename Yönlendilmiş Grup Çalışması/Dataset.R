# Veri setimizi bulunduðu dosya konumundan çekerek tanýmladýk.
getwd()
setwd("C:/Users/arsla/Desktop/Ygç veri seti 3")
getwd()
data= readxl::read_excel("Child_Aggression.xlsx")
data

# veri incelemesi
summary(data)
# summary sonucu veri setindeki tüm deðiþenler için
# merkezi eðilim ölçümleri yapýldý ve tüm 

#basýklýk ve çarpýklýk hesabý
library(moments)
# 0 ise çarpýklýk olmadýðýna iþarettir.
# 0'dan büyük ise saða çarpýktýr.
# 0'dan küçük ise sola çarpýktýr.

skewness(data$Aggression)         # -0.0220
skewness((data$Television))       # -0.3572
skewness(data$Computer_Games)     # 0.2486
skewness(data$Sibling_Aggression) #-0.1690
skewness(data$Diet)               # -0.1219
skewness(data$Parenting_Style)    # -0.2204
# Deðiþkenlerin çarpýklýk kat sayýlarýna bakýldýðýnda çoðunluðun
#sola çarpýk bir durumda olduðunu fakat normalede yakýn olduklarýný görmekteyiz.

#--------x-------x--------x---------x--x------------x

# basýklýk incelemesi
#basýklýk kat sayýsý 0 oldugunda basýklýk olmaz bu durum
#simetrik oldugunu isaret eder.
# 0-3 arasýnda olursa normal daðýlýmdan daha basýktýr ve geniþ bir alana
#yayýlmýþtýr.
#  3'den büyük ise uç deðerlerin olasýlýðý büyüktür.

kurtosis(data$Aggression)        #4.6239
kurtosis(data$Television)        #4.3533
kurtosis(data$Computer_Games)    #4.6096
kurtosis(data$Sibling_Aggression)#4.4123
kurtosis(data$Diet)              #4.5263
kurtosis(data$Parenting_Style)   #4.6887
#deðiþkenlerin basýklýk katsayýlarý incelendiðinde
#hepsinin 3'den büyük olduðu görülmektedir bu durum grafiklerde 
#uç noktalarýn olduðunu göstermektedir.
par(mfrow= c(2:2))
hist(data$Aggression)
hist(data$Television)
hist(data$Computer_Games)
hist(data$Sibling_Aggression)
hist(data$Diet)
hist(data$Parenting_Style)

#------------x----------x-----------x-------------x---------x------
par(mfrow=c(2:1))
boxplot(data$Aggression,horizontal = T,xlab="Aggression") #Aggression veri dagilimi
hist(data$Aggression,xlab="Aggression")

boxplot(data$Television,horizontal = T,xlab="Television") # Television veri dagilimi
hist(data$Television,xlab="Television")

boxplot(data$Computer_Games,horizontal = T,xlab="Computer_Games") #Computer games veri dagilimi
hist(data$Computer_Games,xlab="Computer_Games")

boxplot(data$Sibling_Aggression,horizontal = T,xlab="Sibling_Agression") #Sibling Aggression veri dagilimi
hist(data$Sibling_Aggression,xlab="Sibling_Agression")

boxplot(data$Diet,horizontal = T,xlab="Diet") # Diet veri dagilimi
hist(data$Diet,xlab="Diet")

boxplot(data$Parenting_Style,horizontal = T,xlab="Parenting Style") # Parenting Syle veri dagilimi
hist(data$Parenting_Style,xlab="Parenting Style")

#------------x----------x-----------x------------x----------x------
#Varsayým analizi
#varyanz analizi istatistik bilim dalýnda, grup ortalamalarý ve 
#bunlara baðlý olan iþlemleri analiz etmek için kullanýlan bir istatistiksel
#modellerdir.

# Ilk adim shapiro-wilk testi

shapiro.test(data$Aggression)        # w = 0.977   p-value = 1.135e-08
shapiro.test(data$Television)        # w = 0.979   p-value = 6.643e-08
shapiro.test(data$Computer_Games)    # w = 0.9765  p-value = 7.38e-09
shapiro.test(data$Sibling_Aggression)# W = 0.98131 p-value = 1.644e-07
shapiro.test(data$Diet)              # W = 0.97795 p-value = 1.804e-08
shapiro.test(data$Parenting_Style)   # W = 0.98186 p-value = 2.412e-07
# herbir deðiþken için ayrý ayrý alýnýr.


#Shapiro-wilk testi ; boþ bir hipotez kurarak deðiþkenleri test eder, eðer shapiro
#testin p deðeri belirlediðimiz alpha(0,05 olarak ele alalým) deðerinden büyük 
#ise boþ hipotez red edilmez bu durumda verimizin normal daðýldýðýný anlarýz.

#homojen varyans analizi
#En az 3 baðomsýz deðiþken olduðu zaman uygulanan bir testtir.

#Hipotezimizi kurarak bakýyoruz.
# H0 = tüm deðiþken mü'leri birbirine eþittir.
# H1 = en az 1 mü farklýdýr

attach(data)
model_anaova=aov(Aggression~.,data = data)
summary(model_anaova)

#F-test deðerleri p deðerlerinden büyük olmasý nedeniyle anlamlý gözükmektedir.
#bu durumda Ho hipoteini red edebiliriz.

# T-test deðerleri
t.test(data)

#------------x----------x-----------x-------------x------------x-----

# korelasyon hesabý ile baðýmsýz deðiþkenler arasýnda
# bað olup olmadýðýna bakýlarak baðýmlý deðiþken belirlenir.

data1=cbind(data$Aggression,data$Television,data$Computer_Games,data$Sibling_Aggression,data$Diet,data$Parenting_Style)
colnames(data1) = c("Aggression","Television","Computer","Sibling_Aggression","Diet","Parenting_style")
pairs(data1[,1:6],pch=19,col="red",lower.panel=NULL)
# kolesyon grafiði incelendiðinde verilerin düzgün bir þekilde daðýlmadýðý görülmüþ
# ve baðýmsýz deðiþkenler arasýnda bir iliþki olmadýðýna karar verilmiþtir.

# korelasyon daire grafiði
dev.off()
library(corrplot)
install.packages("corrplot") # kütüphane hatasýnda deneyeceðiz.
Matris=cor(data)
corrplot(Matris,methods="circle")

datax=cbind(data$Parenting_Style,data$Television)
cor(datax) #0.5264

datax=cbind(data$Parenting_Style,data$Computer_Games)
cor(datax) #0.214

datax=cbind(data$Parenting_Style,data$Sibling_Aggression)
cor(datax) #0.173

datax=cbind(data$Parenting_Style,data$Diet)
cor(datax) #0.26

datax=cbind(data$Parenting_Style,data$Aggression)
cor(datax) #0.210

model=lm(data$Parenting_Style~data$Television+data$Computer_Games+data$Sibling_Aggression+data$Aggression+data$Diet)
summary(model)
model$coefficients

#----------x------------------x-------------------x-----------x-----------

#Regresyon
lm.fit=lm(Aggression~.,data = data)
summary(lm.fit)

#VIF deðeri hesaplama ve yorumlama;
car::vif(lm.fit)
# grafik ile inceleme
dev.off()
vif_values=car::vif(lm.fit)
barplot(vif_values, main = "VIF Values",horiz = T,col = "steelblue")
abline(v=5, lwd=2, lty=3)
#Çoklu regresyon sonucuna baktýðýmýzda modelin anlamlý olduðunu
#ve belirtme katsayýsýnýn 0.075 olduðunu görüyoruz.baðýmsýz deðiþkenler baðýlmý 
#deðiþkeni yeterince açýklayamamaktadýr.veri setine girecek yeni baðýmsýz 
#deðiþkenlerle adj.R^2 veri açýklanabilirliði artýrýlabilir.

#modeli daha anlamlý hale getirmek için stepwise 
#yöntemi kullanýyoruz, burada tercih ettiðimiz adým çýkartarak gitmek
#bu adým öncelikle anlamlýlýk deðerini hesaplar, devamýnda tüm deðiþkenleri
#modele sokar ve her baðýmsýz deðiþkenin anlamlýlýk düzeyini belirler
#eðer anlamlýlýk düzeyi model için belirlenenden yüksek ise o baðýmsýz
#deðiþken modelden çýkartýlýr. Þayet birden fazla varsa p deðeri en büyük 
#olan modelden atýlýr.
# Tüm bunlar baðýmsýz deðiþkenlerin p deðeri eþiðinin altýna düþene kadar
#tekrarlanýr.

#Stepwise için paket kurulumu;
library(MASS)
library(tidyverse) # yok ise yükleme iþleme uygulanýr.
install.packages("tidyverse")
library(caret)     # yok ise yükleme iþlemi uygulanýr.
install.packages("caret")
library(leaps)     # yok ise yükleme iþlemi uygulanýr.
install.packages("leaps")

full.model1=lm(Aggression~.,data = data) # regresyon için anlamlý hale 
#getirildi.

step.model1=stepAIC(full.model1,direction = "both",trace = F)
summary(step.model1)
# stepwise modeli kurduk ve uygun hale gelmesini bekledik.

model1= regsubsets(Parenting_Style~.,data = data,nvmax = 6,method = "seqrep")
summary(model)

# seqrep : modeller arasýnda ileri geri yapmasýna yardýmcý olan yönetim 
#modunu verir.

# nymax: en iyi modeli oluþturabilmesi için öngörücü olmasý gerektiðini
#belirtir.

set.seed(123) # tekrar modeller
train.control1= trainControl(method = "cv",number=10)
#tekrarlanan k çapraz doðrulama kuruyor.

step.model = train(Aggression ~., data = data,
                   method= "leapForward",
                   tuneGrid= data.frame(nvmax= 1:6),
                   trControl= train.control1
                   )
# method'a doðrusal regresyonda geriye doðru seçim yapmasý gerektiðini
#belirttik.
# bu kod yardýmý ile en iyi 5 modeli karþýlastýrmýþ oluyoruz.

step.model$results
#en iyi 5 modelin çýktýsýný verir ve bu modellerin karþýlaþtýrmak için farklý 
#ölçümleri ve bunlarýn standart sapmalarýný verir.

step.model$bestTune
# bu kod yardýmý ile de en iyi modelin kaç deðiþkenli oldupunu belirtir
# sonuç olarak x tane deðiþkenli model daha doðrudur diyebiliriz.

summary(step.model$finalModel)
#bu kod bize bir tablo vermektedir, bu tabloda yer alan yýldýzlar bize
#kaçýncý model denemesinde hangi deðiþkeni ele aldýðýný göstermektedir.
# tabloya göz atýldýðýnda Televizyonun Agresifliði etkilemediði
#belirlenerek modelden çýkartýlmýþ ve daha anlamlý bir model elde edilmiþ.

coef(step.model$finalModel,4)
# sonuç olan modelin regresyon katsayýlarý bu komut ile elde ediyoruz.

lm(Aggression~Parenting_Style+Sibling_Aggression+Computer_Games+Diet,data = data)
# son olarak doðrusal model öngörülen deðiþkenler ile hesaplanýr.





