getwd()
setwd("C:/Users/arsla/Desktop/4.Sinif Dersleri/cdi/Proje")
#veri setini kaydettiðiniz dosyanýn yolunu üstteki koda yapýþtýrýn ve Ters 
#taksim iþaretini düz taksim haline getirip öyle çalýþtýrýn.
data=readxl::read_excel("Veri.xlsx")
data
#Eksik olan ya da gerekli paketlerin indirme iþlemi.
install.packages("corrr")# Korelasyon analizi için kullanýlan bir pakettir.
install.packages("ggcorrplot") # Korelasyon matrisini görselleþtirmemizde yardýmcý olur.
install.packages("FactoMineR") #PCA modülüne eriþim saðlamaktadýr.
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
#Paketlerinçaðrýlma iþlemi.
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
#Veri seti ile tanýþma ve Varsayýmlarýn kontrölü gerçekleþtirme.
data # veri setinin görünümü bizlere gösterir.
str(data) # veri seti hakkýnda bilgi aktarýr.
colSums(is.na(data)) # Her bir deðiþken için kayýp deðer taramasý yapar.
summary(data) # veri setinin özet istatistiðini bizlere verir.


data=data[,-1]
#---------- MVN ---------------
str(data)
scaleli=scale(data[,-1])
result = mvn(data = data[,-c(1,2,3)],mvnTest = "royston")
#yüksek deðerlere sahip deðiþkenler mvn dýþý olarak alýnca çalýþýyor.
result

mvn(data = data[,-c(1,2,3)],mvnTest = "royston",cov=TRUE,multivariatePlot = "qq")
# -------- Korelaston matrisi Grafik ------

str(data)
numeric_veri=data[,2:18]
head(numeric_veri) # Standartlaþtýrma iþleminden önce numeric türe sahip olan 
#deðiþkenleri yeni bir veri setinde tanýmlýyoruz. Böylelikle String deðere sahip
#olan deðiþkenleri dýþarda býrakmýþ oluyoruz.

normal_veri=scale(numeric_veri)
head(normal_veri) #tüm deðiþkenleri tek bir ölçekde toplamýþ bulunduk.

corr_veri=cor(normal_veri)
ggcorrplot(corr_veri)

#------ Varyans homejenliði ---------

cortest.bartlett(corr_veri,n =nrow(numeric_veri)) 

#---- KMO Testi ------- 

KMO(numeric_veri)

#------ Aykýrý deðer kontrolü------

par(mfrow=(c(3,6)))
boxplot(data$Konut_satis_sayisi,main="Konut satis sayýsý",horizontal = TRUE,range = 3)
#Konut satýþ outliner var.
boxplot(data$insa_yili_1980_den_once_olan_konut_orani,main="1980 yýlýndan once insa edilen konut oraný",horizontal = TRUE,range = 3)
boxplot(data$insa_yili_1981_2000_arasinda_olan_konut_orani,main="1981-2000 yillari arasinde insa edilen konutlar",horizontal = TRUE,range = 3)
boxplot(data$insa_yili_2001_den_sonra_olan_konut_orani ,main="2001 yýlýndan sonra insa edilen konutlar",horizontal = TRUE,range = 3)
boxplot(data$insa_yili_bilinmeyen_konutlarin_orani,main="insa yýlý bilinmeyen konutlar",horizontal = TRUE,range = 3)
boxplot(data$Kati_yakit_kullanan_konut_orani,main="Katý yakýt kullanan konut oraný",horizontal = TRUE,range = 3)
#outliner var.
boxplot(data$Dogalgaz_kullanan_konutlarin_orani,main="Dogalgaz kullanan konut oraný",horizontal = TRUE,range = 3)
boxplot(data$Digerlerini_kullanan_konutlarin_orani,main="diger yakýtlarý kullanan konut oraný",horizontal = TRUE,range = 3)
#Diðer yakýtlarda outliner var.
boxplot(data$Asansor_bulunan_konutlarin_orani,main="Asansoru bulunan konutlarýn oraný",horizontal = TRUE,range = 3)
boxplot(data$Asansor_bulunmayan_konutlarin_orani,main="Asansoru bulunmayan konutlarýn oraný",horizontal = TRUE,range = 3)
boxplot(data$Illerde_bulunan_Hastane_sayilari ,main="illerde bulunan hastane sayýsý",horizontal = TRUE,range = 3)
#Hastane Sayýsý
boxplot(data$Erkeklere_satilan_konut_sayisi,main="Erkeklere satilan konut sayýsý",horizontal = TRUE,range = 3)
#Erkeklere satýþ
boxplot(data$Kadinlara_satilan_konut_sayisi,main="Kadinlara satýlan konut sayýsý",horizontal = TRUE,range = 3)
#Kadýna satýþ var.
boxplot(data$Ortak_olarak_satilan_konut_sayisi,main="Ortak olarak satýlan konut sayýsý",horizontal = TRUE,range = 3)
#ortak satýþta var
boxplot(data$Diger_sahislara_satilan_konut_sayisi,main="Diger sahislara satýlan konut sayýsý",horizontal = TRUE,range = 3)
#diðerlere satýþda var.
boxplot(data$Fert_basina_dusen_oda_sayisi_ortalama,main="Fert basina düþen oda sayýsý",horizontal = TRUE,range = 3)
boxplot(data$Nufus_yogunlugu,main="Nufus Yogunlugu",horizontal = TRUE,range = 3)

#------LN Dönüþüm-----------------

data$Konut_satis_sayisi=log(data$Konut_satis_sayisi)
data$Digerlerini_kullanan_konutlarin_orani=log(data$Digerlerini_kullanan_konutlarin_orani)
data$Illerde_bulunan_Hastane_sayilari=log(data$Illerde_bulunan_Hastane_sayilari)
data$Erkeklere_satilan_konut_sayisi=log(data$Erkeklere_satilan_konut_sayisi)
data$Kadinlara_satilan_konut_sayisi=log(data$Kadinlara_satilan_konut_sayisi)
data$Ortak_olarak_satilan_konut_sayisi=log(data$Ortak_olarak_satilan_konut_sayisi)
data$Diger_sahislara_satilan_konut_sayisi=log(data$Diger_sahislara_satilan_konut_sayisi)
data$Nufus_yogunlugu=log(data$Nufus_yogunlugu)

#---------Dönüþüm sonrasý Outlier Kontrolü------------

dev.off()
par(mfrow=(c(3,6)))
boxplot(data$Konut_satis_sayisi,main="Konut satis sayýsý",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$insa_yili_1980_den_once_olan_konut_orani,main="1980 yýlýndan once insa edilen konut oraný",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$insa_yili_1981_2000_arasinda_olan_konut_orani,main="1981-2000 yillari arasinde insa edilen konutlar",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$insa_yili_2001_den_sonra_olan_konut_orani ,main="2001 yýlýndan sonra insa edilen konutlar",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$insa_yili_bilinmeyen_konutlarin_orani,main="insa yýlý bilinmeyen konutlar",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Kati_yakit_kullanan_konut_orani,main="Katý yakýt kullanan konut oraný",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Dogalgaz_kullanan_konutlarin_orani,main="Dogalgaz kullanan konut oraný",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Digerlerini_kullanan_konutlarin_orani,main="diger yakýtlarý kullanan konut oraný",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Asansor_bulunan_konutlarin_orani,main="Asansoru bulunan konutlarýn oraný",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Asansor_bulunmayan_konutlarin_orani,main="Asansoru bulunmayan konutlarýn oraný",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Illerde_bulunan_Hastane_sayilari ,main="illerde bulunan hastane sayýsý",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Erkeklere_satilan_konut_sayisi,main="Erkeklere satilan konut sayýsý",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Kadinlara_satilan_konut_sayisi,main="Kadinlara satýlan konut sayýsý",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Ortak_olarak_satilan_konut_sayisi,main="Ortak olarak satýlan konut sayýsý",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Diger_sahislara_satilan_konut_sayisi,main="Diger sahislara satýlan konut sayýsý",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Fert_basina_dusen_oda_sayisi_ortalama,main="Fert basina düþen oda sayýsý",horizontal = TRUE,range = 3,col = "#b961d8")
boxplot(data$Nufus_yogunlugu,horizontal = TRUE,range = 3,main="Nufus Yogunlugu",col = "#b961d8")



# ----------------PCA uygulama adýmý ----------------
PCA_veri=princomp(corr_veri)
summary(PCA_veri) # kümülatif açýklýyýcýlýklarýna bakarak 4 deðiþkende olmasý 
#ile deðiþkenlerin 0.97 sini açýklandýðý gözlemlenmiþtir. Bu karara dayanarak
#deðiþken sayýsýnýn 4'e düþürülmüþtür.

yepyeni=PCA_veri$loadings[, 1:4]
yepyeni
#Scree Plot 
fviz_eig(PCA_veri, addlabels = TRUE) #scree plot yardýmý ile deðiþken sayýný 
#kaça düþüreceðimizi belirlemiþ hatta daha önce belilediðimiz deðeride 
#destekleyebiliriz.

#Niteliklerin Grafiði
# Graph of the variables
fviz_pca_var(PCA_veri, col.var = "black")

#DEðiþkenlerin biribirini temsil etme deðerleri
fviz_cos2(PCA_veri, choice = "var", axes = 1:2)
fviz_pca_var(PCA_veri, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

# ----------Faktör Analizi--------------- 

#Verinin Ölçeklendirilmesi
#Daha önce string deðer olarak atadaðýmýz veriyi=numeric_veri üzerinden devam
#edilmiþtir.
#Faktör sayýsýnýn belirlenmesi
fa = fa(r = data[,-1], 
         nfactors = 4, 
         rotate = "varimax") 
summary(fa) 
fa$e.values>1#Özdeðerleri 1'den büyük deðerler belirlenmiþtir. Belirlenen Bu 
#deðerlerin 4 adet olduðu gözlemlenmiþtir. Ve model tekrardan oluþturulmuþtur.
fa$loadings
fa$scores
#Grafiði var mý diye araþtýrýlacak?
# Extract factor loadings from the model
loadings = fa$loadings
loadings

colnames(loadings) #Veri setindeki Deðiþken isimlerini Görüntüler.
yeni_isim=c("Sehir_Yasam_Faktoru","Yapý_Insa_Tarihi_Faktoru",
            "Konut_Tipi_Satis_Faktoru",
            "Isi_Kaynagina_Gore_Satis_Faktoru")
colnames(loadings) = yeni_isim
dev.off()
fa.diagram(loadings)

theta=varimax(loadings)$rotmat
l_rotate=loadings%*%theta
l_rotate # yüklerin tamamýný veriyor.

#Score ile deneme
fa_veri=fa$scores
colnames(fa_veri)=yeni_isim
fa_veri
# veri birleþtirme
merge_veri=merge(data$Konut_satis_kategorisi,fa_veri,by="row.names",all=FALSE)
merge_veri=merge_veri[,-1]
str(merge_veri)
merge_veri

summary(merge_veri)

write.xlsx(merge_veri, "Faktor_verisi.xlsx", 
           sheetName = "Sheet1", 
           rowNames = FALSE) # faktör analizi ile deðiþkenleri indirgenmiþ
#veriyi yeni bir excel dosyasý olarak kaydettik ardýndan excel üzerinden katedorik 
#deðiþken eklemesi yapýlmýþtýr.


# ------ Faktör MVN ------ 
result2 = mvn(data = merge_veri[,-1],mvnTest = "royston")
result2 # faktör analizi yapýldýktan sonra Çoklu normallik kontrolü yapýlmýþ ve 
#Normalliðe uymadýðý kararlaþtýrýlmýþtýr.


#--------- Lojistik Regresyon -------
table(merge_veri$x)

# Train - Test verisi
trate=sample(c(TRUE, FALSE),nrow(merge_veri),replace = TRUE, prob = c(0.8,0.2) )
train=merge_veri[trate,]
test=merge_veri[!trate,]
#model oluþturma
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

# Küme sayýsý belirlendikten sonra kümeleme iþlemi yapýlýr.
km2<-eclust(results, "kmeans", hc_metric="manhattan",k=2)

# --------- Hiyerarþik Kümeleme -------------
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

#------------ Çok Boyutlu Örnekle Analizi ---------
dev.off()
str(data)
d=dist(data)
fit = cmdscale(d,eig = TRUE,k=3)
fit
   
  #Stres kontrolü
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

