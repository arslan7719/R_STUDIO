# Veri setimizi bulundu�u dosya konumundan �ekerek tan�mlad�k.
getwd()
setwd("C:/Users/arsla/Desktop/Yg� veri seti 3")
getwd()
data= readxl::read_excel("Child_Aggression.xlsx")
data

# veri incelemesi
summary(data)
# summary sonucu veri setindeki t�m de�i�enler i�in
# merkezi e�ilim �l��mleri yap�ld� ve t�m 

#bas�kl�k ve �arp�kl�k hesab�
library(moments)
# 0 ise �arp�kl�k olmad���na i�arettir.
# 0'dan b�y�k ise sa�a �arp�kt�r.
# 0'dan k���k ise sola �arp�kt�r.

skewness(data$Aggression)         # -0.0220
skewness((data$Television))       # -0.3572
skewness(data$Computer_Games)     # 0.2486
skewness(data$Sibling_Aggression) #-0.1690
skewness(data$Diet)               # -0.1219
skewness(data$Parenting_Style)    # -0.2204
# De�i�kenlerin �arp�kl�k kat say�lar�na bak�ld���nda �o�unlu�un
#sola �arp�k bir durumda oldu�unu fakat normalede yak�n olduklar�n� g�rmekteyiz.

#--------x-------x--------x---------x--x------------x

# bas�kl�k incelemesi
#bas�kl�k kat say�s� 0 oldugunda bas�kl�k olmaz bu durum
#simetrik oldugunu isaret eder.
# 0-3 aras�nda olursa normal da��l�mdan daha bas�kt�r ve geni� bir alana
#yay�lm��t�r.
#  3'den b�y�k ise u� de�erlerin olas�l��� b�y�kt�r.

kurtosis(data$Aggression)        #4.6239
kurtosis(data$Television)        #4.3533
kurtosis(data$Computer_Games)    #4.6096
kurtosis(data$Sibling_Aggression)#4.4123
kurtosis(data$Diet)              #4.5263
kurtosis(data$Parenting_Style)   #4.6887
#de�i�kenlerin bas�kl�k katsay�lar� incelendi�inde
#hepsinin 3'den b�y�k oldu�u g�r�lmektedir bu durum grafiklerde 
#u� noktalar�n oldu�unu g�stermektedir.
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
#Varsay�m analizi
#varyanz analizi istatistik bilim dal�nda, grup ortalamalar� ve 
#bunlara ba�l� olan i�lemleri analiz etmek i�in kullan�lan bir istatistiksel
#modellerdir.

# Ilk adim shapiro-wilk testi

shapiro.test(data$Aggression)        # w = 0.977   p-value = 1.135e-08
shapiro.test(data$Television)        # w = 0.979   p-value = 6.643e-08
shapiro.test(data$Computer_Games)    # w = 0.9765  p-value = 7.38e-09
shapiro.test(data$Sibling_Aggression)# W = 0.98131 p-value = 1.644e-07
shapiro.test(data$Diet)              # W = 0.97795 p-value = 1.804e-08
shapiro.test(data$Parenting_Style)   # W = 0.98186 p-value = 2.412e-07
# herbir de�i�ken i�in ayr� ayr� al�n�r.


#Shapiro-wilk testi ; bo� bir hipotez kurarak de�i�kenleri test eder, e�er shapiro
#testin p de�eri belirledi�imiz alpha(0,05 olarak ele alal�m) de�erinden b�y�k 
#ise bo� hipotez red edilmez bu durumda verimizin normal da��ld���n� anlar�z.

#homojen varyans analizi
#En az 3 ba�oms�z de�i�ken oldu�u zaman uygulanan bir testtir.

#Hipotezimizi kurarak bak�yoruz.
# H0 = t�m de�i�ken m�'leri birbirine e�ittir.
# H1 = en az 1 m� farkl�d�r

attach(data)
model_anaova=aov(Aggression~.,data = data)
summary(model_anaova)

#F-test de�erleri p de�erlerinden b�y�k olmas� nedeniyle anlaml� g�z�kmektedir.
#bu durumda Ho hipoteini red edebiliriz.

# T-test de�erleri
t.test(data)

#------------x----------x-----------x-------------x------------x-----

# korelasyon hesab� ile ba��ms�z de�i�kenler aras�nda
# ba� olup olmad���na bak�larak ba��ml� de�i�ken belirlenir.

data1=cbind(data$Aggression,data$Television,data$Computer_Games,data$Sibling_Aggression,data$Diet,data$Parenting_Style)
colnames(data1) = c("Aggression","Television","Computer","Sibling_Aggression","Diet","Parenting_style")
pairs(data1[,1:6],pch=19,col="red",lower.panel=NULL)
# kolesyon grafi�i incelendi�inde verilerin d�zg�n bir �ekilde da��lmad��� g�r�lm��
# ve ba��ms�z de�i�kenler aras�nda bir ili�ki olmad���na karar verilmi�tir.

# korelasyon daire grafi�i
dev.off()
library(corrplot)
install.packages("corrplot") # k�t�phane hatas�nda deneyece�iz.
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

#VIF de�eri hesaplama ve yorumlama;
car::vif(lm.fit)
# grafik ile inceleme
dev.off()
vif_values=car::vif(lm.fit)
barplot(vif_values, main = "VIF Values",horiz = T,col = "steelblue")
abline(v=5, lwd=2, lty=3)
#�oklu regresyon sonucuna bakt���m�zda modelin anlaml� oldu�unu
#ve belirtme katsay�s�n�n 0.075 oldu�unu g�r�yoruz.ba��ms�z de�i�kenler ba��lm� 
#de�i�keni yeterince a��klayamamaktad�r.veri setine girecek yeni ba��ms�z 
#de�i�kenlerle adj.R^2 veri a��klanabilirli�i art�r�labilir.

#modeli daha anlaml� hale getirmek i�in stepwise 
#y�ntemi kullan�yoruz, burada tercih etti�imiz ad�m ��kartarak gitmek
#bu ad�m �ncelikle anlaml�l�k de�erini hesaplar, devam�nda t�m de�i�kenleri
#modele sokar ve her ba��ms�z de�i�kenin anlaml�l�k d�zeyini belirler
#e�er anlaml�l�k d�zeyi model i�in belirlenenden y�ksek ise o ba��ms�z
#de�i�ken modelden ��kart�l�r. �ayet birden fazla varsa p de�eri en b�y�k 
#olan modelden at�l�r.
# T�m bunlar ba��ms�z de�i�kenlerin p de�eri e�i�inin alt�na d��ene kadar
#tekrarlan�r.

#Stepwise i�in paket kurulumu;
library(MASS)
library(tidyverse) # yok ise y�kleme i�leme uygulan�r.
install.packages("tidyverse")
library(caret)     # yok ise y�kleme i�lemi uygulan�r.
install.packages("caret")
library(leaps)     # yok ise y�kleme i�lemi uygulan�r.
install.packages("leaps")

full.model1=lm(Aggression~.,data = data) # regresyon i�in anlaml� hale 
#getirildi.

step.model1=stepAIC(full.model1,direction = "both",trace = F)
summary(step.model1)
# stepwise modeli kurduk ve uygun hale gelmesini bekledik.

model1= regsubsets(Parenting_Style~.,data = data,nvmax = 6,method = "seqrep")
summary(model)

# seqrep : modeller aras�nda ileri geri yapmas�na yard�mc� olan y�netim 
#modunu verir.

# nymax: en iyi modeli olu�turabilmesi i�in �ng�r�c� olmas� gerekti�ini
#belirtir.

set.seed(123) # tekrar modeller
train.control1= trainControl(method = "cv",number=10)
#tekrarlanan k �apraz do�rulama kuruyor.

step.model = train(Aggression ~., data = data,
                   method= "leapForward",
                   tuneGrid= data.frame(nvmax= 1:6),
                   trControl= train.control1
                   )
# method'a do�rusal regresyonda geriye do�ru se�im yapmas� gerekti�ini
#belirttik.
# bu kod yard�m� ile en iyi 5 modeli kar��last�rm�� oluyoruz.

step.model$results
#en iyi 5 modelin ��kt�s�n� verir ve bu modellerin kar��la�t�rmak i�in farkl� 
#�l��mleri ve bunlar�n standart sapmalar�n� verir.

step.model$bestTune
# bu kod yard�m� ile de en iyi modelin ka� de�i�kenli oldupunu belirtir
# sonu� olarak x tane de�i�kenli model daha do�rudur diyebiliriz.

summary(step.model$finalModel)
#bu kod bize bir tablo vermektedir, bu tabloda yer alan y�ld�zlar bize
#ka��nc� model denemesinde hangi de�i�keni ele ald���n� g�stermektedir.
# tabloya g�z at�ld���nda Televizyonun Agresifli�i etkilemedi�i
#belirlenerek modelden ��kart�lm�� ve daha anlaml� bir model elde edilmi�.

coef(step.model$finalModel,4)
# sonu� olan modelin regresyon katsay�lar� bu komut ile elde ediyoruz.

lm(Aggression~Parenting_Style+Sibling_Aggression+Computer_Games+Diet,data = data)
# son olarak do�rusal model �ng�r�len de�i�kenler ile hesaplan�r.





