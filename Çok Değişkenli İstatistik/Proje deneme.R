getwd()
setwd("C:/Users/arsla/Desktop/4.Sinif Dersleri/cdi/Proje")
#veri setini kaydetti�iniz dosyan�n yolunu �stteki koda yap��t�r�n ve Ters 
#taksim i�aretini d�z taksim haline getirip �yle �al��t�r�n.
data=readxl::read_excel("Veri.xlsx")
data
#Eksik olan ya da gerekli paketlerin indirme i�lemi.
install.packages("corrr")# Korelasyon analizi i�in kullan�lan bir pakettir.
install.packages("ggcorrplot") # Korelasyon matrisini g�rselle�tirmemizde yard�mc� olur.
install.packages("FactoMineR") #PCA mod�l�ne eri�im sa�lamaktad�r.
install.packages("factoextra")
install.packages("stats")
install.packages("cortest")
install.packages("psych")
install.packages("glmnet")
install.packages("hopkins")
install.packages("cluster")
install.packages("magrittr")
install.packages("dendextend")
install.packages("reshape")
install.packages("Factoshiny")
install.packages("openxlsx")
install.packages("pscl")
#Paketlerin�a�r�lma i�lemi.
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(stats)
library(corTest)
library(psych)
library(MASS)
library(tidyverse)
library(caret)
library(MVN)
library(glmnet)
library(hopkins)
library(cluster)
library(magrittr)
library(dendextend)
library(reshape)
library(Factoshiny)
library(openxlsx)
library(pscl)
#Veri seti ile tan��ma ve Varsay�mlar�n kontr�l� ger�ekle�tirme.
data # veri setinin g�r�n�m� bizlere g�sterir.
str(data) # veri seti hakk�nda bilgi aktar�r.
colSums(is.na(data)) # Her bir de�i�ken i�in kay�p de�er taramas� yapar.
summary(data) # veri setinin �zet istatisti�ini bizlere verir.


data=data[,-1]
#---------- MVN ---------------
str(data)
scaleli=scale(data[,-1])
result = mvn(data = data[,-c(1,2,3)],mvnTest = "royston")
#y�ksek de�erlere sahip de�i�kenler mvn d��� olarak al�nca �al���yor.
result

mvn(data = data[,-c(1,2,3)],mvnTest = "royston",cov=TRUE,multivariatePlot = "qq")
# -------- Korelaston matrisi Grafik ------

str(data)
numeric_veri=data[,2:18]
head(numeric_veri) # Standartla�t�rma i�leminden �nce numeric t�re sahip olan 
#de�i�kenleri yeni bir veri setinde tan�ml�yoruz. B�ylelikle String de�ere sahip
#olan de�i�kenleri d��arda b�rakm�� oluyoruz.

normal_veri=scale(numeric_veri)
head(normal_veri) #t�m de�i�kenleri tek bir �l�ekde toplam�� bulunduk.

corr_veri=cor(normal_veri)
ggcorrplot(corr_veri)

#------ Varyans homejenli�i ---------

cortest.bartlett(corr_veri,n =nrow(numeric_veri)) 

#---- KMO Testi ------- 

KMO(numeric_veri)

#------ Ayk�r� de�er kontrol�------

par(mfrow=(c(3,6)))
boxplot(data$Konut_satis_sayisi,main="Konut satis say�s�",horizontal = TRUE,range = 3)
#Konut sat�� outliner var.
boxplot(data$insa_yili_1980_den_once_olan_konut_orani,main="1980 y�l�ndan once insa edilen konut oran�",horizontal = TRUE,range = 3)
boxplot(data$insa_yili_1981_2000_arasinda_olan_konut_orani,main="1981-2000 yillari arasinde insa edilen konutlar",horizontal = TRUE,range = 3)
boxplot(data$insa_yili_2001_den_sonra_olan_konut_orani ,main="2001 y�l�ndan sonra insa edilen konutlar",horizontal = TRUE,range = 3)
boxplot(data$insa_yili_bilinmeyen_konutlarin_orani,main="insa y�l� bilinmeyen konutlar",horizontal = TRUE,range = 3)
boxplot(data$Kati_yakit_kullanan_konut_orani,main="Kat� yak�t kullanan konut oran�",horizontal = TRUE,range = 3)
#outliner var.
boxplot(data$Dogalgaz_kullanan_konutlarin_orani,main="Dogalgaz kullanan konut oran�",horizontal = TRUE,range = 3)
boxplot(data$Digerlerini_kullanan_konutlarin_orani,main="diger yak�tlar� kullanan konut oran�",horizontal = TRUE,range = 3)
#Di�er yak�tlarda outliner var.
boxplot(data$Asansor_bulunan_konutlarin_orani,main="Asansoru bulunan konutlar�n oran�",horizontal = TRUE,range = 3)
boxplot(data$Asansor_bulunmayan_konutlarin_orani,main="Asansoru bulunmayan konutlar�n oran�",horizontal = TRUE,range = 3)
boxplot(data$Illerde_bulunan_Hastane_sayilari ,main="illerde bulunan hastane say�s�",horizontal = TRUE,range = 3)
#Hastane Say�s�
boxplot(data$Erkeklere_satilan_konut_sayisi,main="Erkeklere satilan konut say�s�",horizontal = TRUE,range = 3)
#Erkeklere sat��
boxplot(data$Kadinlara_satilan_konut_sayisi,main="Kadinlara sat�lan konut say�s�",horizontal = TRUE,range = 3)
#Kad�na sat�� var.
boxplot(data$Ortak_olarak_satilan_konut_sayisi,main="Ortak olarak sat�lan konut say�s�",horizontal = TRUE,range = 3)
#ortak sat��ta var
boxplot(data$Diger_sahislara_satilan_konut_sayisi,main="Diger sahislara sat�lan konut say�s�",horizontal = TRUE,range = 3)
#di�erlere sat��da var.
boxplot(data$Fert_basina_dusen_oda_sayisi_ortalama,main="Fert basina d��en oda say�s�",horizontal = TRUE,range = 3)
boxplot(data$Nufus_yogunlugu,main="Nufus Yogunlugu",horizontal = TRUE,range = 3)

#------LN D�n���m-----------------

data$Konut_satis_sayisi=log(data$Konut_satis_sayisi)
data$Digerlerini_kullanan_konutlarin_orani=log(data$Digerlerini_kullanan_konutlarin_orani)
data$Illerde_bulunan_Hastane_sayilari=log(data$Illerde_bulunan_Hastane_sayilari)
data$Erkeklere_satilan_konut_sayisi=log(data$Erkeklere_satilan_konut_sayisi)
data$Kadinlara_satilan_konut_sayisi=log(data$Kadinlara_satilan_konut_sayisi)
data$Ortak_olarak_satilan_konut_sayisi=log(data$Ortak_olarak_satilan_konut_sayisi)
data$Diger_sahislara_satilan_konut_sayisi=log(data$Diger_sahislara_satilan_konut_sayisi)
data$Nufus_yogunlugu=log(data$Nufus_yogunlugu)

#---------D�n���m sonras� Outlier Kontrol�------------

dev.off()
par(mfrow=(c(3,6)))
boxplot(data$Konut_satis_sayisi,main="Konut satis say�s�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$insa_yili_1980_den_once_olan_konut_orani,main="1980 y�l�ndan once insa edilen konut oran�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$insa_yili_1981_2000_arasinda_olan_konut_orani,main="1981-2000 yillari arasinde insa edilen konutlar",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$insa_yili_2001_den_sonra_olan_konut_orani ,main="2001 y�l�ndan sonra insa edilen konutlar",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$insa_yili_bilinmeyen_konutlarin_orani,main="insa y�l� bilinmeyen konutlar",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Kati_yakit_kullanan_konut_orani,main="Kat� yak�t kullanan konut oran�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Dogalgaz_kullanan_konutlarin_orani,main="Dogalgaz kullanan konut oran�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Digerlerini_kullanan_konutlarin_orani,main="diger yak�tlar� kullanan konut oran�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Asansor_bulunan_konutlarin_orani,main="Asansoru bulunan konutlar�n oran�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Asansor_bulunmayan_konutlarin_orani,main="Asansoru bulunmayan konutlar�n oran�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Illerde_bulunan_Hastane_sayilari ,main="illerde bulunan hastane say�s�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Erkeklere_satilan_konut_sayisi,main="Erkeklere satilan konut say�s�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Kadinlara_satilan_konut_sayisi,main="Kadinlara sat�lan konut say�s�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Ortak_olarak_satilan_konut_sayisi,main="Ortak olarak sat�lan konut say�s�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Diger_sahislara_satilan_konut_sayisi,main="Diger sahislara sat�lan konut say�s�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Fert_basina_dusen_oda_sayisi_ortalama,main="Fert basina d��en oda say�s�",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Nufus_yogunlugu,horizontal = TRUE,range = 3,main="Nufus Yogunlugu",col = "#b961d8")



# ----------------PCA uygulama ad�m� ----------------
PCA_veri=princomp(corr_veri)
summary(PCA_veri) # k�m�latif a��kl�y�c�l�klar�na bakarak 4 de�i�kende olmas� 
#ile de�i�kenlerin 0.97 sini a��kland��� g�zlemlenmi�tir. Bu karara dayanarak
#de�i�ken say�s�n�n 4'e d���r�lm��t�r.

yepyeni=PCA_veri$loadings[, 1:4]
yepyeni
#Scree Plot 
fviz_eig(PCA_veri, addlabels = TRUE) #scree plot yard�m� ile de�i�ken say�n� 
#ka�a d���rece�imizi belirlemi� hatta daha �nce beliledi�imiz de�eride 
#destekleyebiliriz.

#Niteliklerin Grafi�i
# Graph of the variables
fviz_pca_var(PCA_veri, col.var = "black")

#DE�i�kenlerin biribirini temsil etme de�erleri
fviz_cos2(PCA_veri, choice = "var", axes = 1:2)
fviz_pca_var(PCA_veri, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

# ----------Fakt�r Analizi--------------- 

#Verinin �l�eklendirilmesi
#Daha �nce string de�er olarak atada��m�z veriyi=numeric_veri �zerinden devam
#edilmi�tir.
#Fakt�r say�s�n�n belirlenmesi
fa = fa(r = data[,-1], 
         nfactors = 4, 
         rotate = "varimax") 
summary(fa) 
fa$e.values>1#�zde�erleri 1'den b�y�k de�erler belirlenmi�tir. Belirlenen Bu 
#de�erlerin 4 adet oldu�u g�zlemlenmi�tir. Ve model tekrardan olu�turulmu�tur.
fa$loadings
fa$scores
#Grafi�i var m� diye ara�t�r�lacak?
# Extract factor loadings from the model
loadings = fa$loadings
loadings

colnames(loadings) #Veri setindeki De�i�ken isimlerini G�r�nt�ler.
yeni_isim=c("Sehir_Yasam_Faktoru","Yap�_Insa_Tarihi_Faktoru",
            "Konut_Tipi_Satis_Faktoru",
            "Isi_Kaynagina_Gore_Satis_Faktoru")
colnames(loadings) = yeni_isim
dev.off()
fa.diagram(loadings)

theta=varimax(loadings)$rotmat
l_rotate=loadings%*%theta
l_rotate # y�klerin tamam�n� veriyor.

#Score ile deneme
fa_veri=fa$scores
colnames(fa_veri)=yeni_isim
fa_veri
# veri birle�tirme
merge_veri=merge(data$Konut_satis_kategorisi,fa_veri,by="row.names",all=FALSE)
merge_veri=merge_veri[,-1]
str(merge_veri)
merge_veri

summary(merge_veri)

write.xlsx(merge_veri, "Faktor_verisi.xlsx", 
           sheetName = "Sheet1", 
           rowNames = FALSE) # fakt�r analizi ile de�i�kenleri indirgenmi�
#veriyi yeni bir excel dosyas� olarak kaydettik ard�ndan excel �zerinden katedorik 
#de�i�ken eklemesi yap�lm��t�r.


# ------ Fakt�r MVN ------ 
result2 = mvn(data = merge_veri[,-1],mvnTest = "royston")
result2 # fakt�r analizi yap�ld�ktan sonra �oklu normallik kontrol� yap�lm�� ve 
#Normalli�e uymad��� kararla�t�r�lm��t�r.


#--------- Lojistik Regresyon -------
table(merge_veri$x)

# Train - Test verisi
trate=sample(c(TRUE, FALSE),nrow(merge_veri),replace = TRUE, prob = c(0.8,0.2) )
train=merge_veri[trate,]
test=merge_veri[!trate,]
#model olu�turma
model3 <- glm(x ~ ., 
             data = train, 
             family = "binomial")

summary(model3)

pscl::pR2(model3)["McFadden"]

#--------- K-means (pca)-------------
xxx.pca1<-prcomp(data, center=TRUE, scale.=TRUE, rank. = 4) # 
xxx.pca1$x
results <- xxx.pca1$x

fviz_nbclust(results,
             FUNcluster=kmeans,
             k.max = 25,
             method = "silhouette") 

# K�me say�s� belirlendikten sonra k�meleme i�lemi yap�l�r.
km2<-eclust(results, "kmeans", hc_metric="manhattan",k=2)

# --------- Hiyerar�ik K�meleme -------------
data[,-1] %>% 
  dist() %>% 
  hclust(method ="complete" ) %>% 
  as.dendrogram() -> dend

par(mar=c(9,1,1,1))
dend %>%
  set("labels_col", value = c("#fa5963", "#3f843e"), k=2) %>%
  set("branches_k_color", value = c("#fa5963", "#3f843e"), k = 2) %>%
  plot(axes=FALSE)
rect.dendrogram( dend, k=2, lty = 5, lwd = 0, x=1, col=rgb(0.1, 0.7, 0.9, 0.1) ) 
rect.dendrogram( dend, k=2, lty = 5, lwd = 0, x=19, col=rgb(0.1, 0.7, 0.9, 0.1) )  

#------------ �ok Boyutlu �rnekle Analizi ---------
dev.off()
str(data)
d=dist(data)
fit = cmdscale(d,eig = TRUE,k=3)
fit
   
  #Stres kontrol�
  stress_deger=fit$GOF
  stress_deger

x1=fit$points[,1]
y1=fit$points[,2]

plot(x1, y1, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x1, y1, labels = row.names(data), cex=.7)

num_clusters <- 2
kmeans_result <- kmeans(fit$points, centers = num_clusters)
kmeans_result$cluster

plot(x1, y1, xlab="Coordinate 1", ylab="Coordinate 2",
     main="K-means Clustering on MDS Results", col=kmeans_result$cluster, pch=15)
text(x1, y1, labels = row.names(data), cex=.6)

