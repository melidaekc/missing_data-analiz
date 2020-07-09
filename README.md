# missing_data-analiz

VERİNİN TANITILMASI
Bağımlı Değişken:

price: ABD Doları olarak fiyat fiyatı

Açıklayıcı Değişkenlerimiz:

caret: elmasın karat ağırlığı

cut: kesim kesim kalitesi (Adil, İyi, Çok İyi, Premium, İdeal)

color: renkli elmas rengi, J'den (en kötü) D'ye (en iyi)

clarity: elmasın ne kadar berrak olduğunun bir netliği (I1 (en kötü), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (en iyi))

depth: derinlik toplam derinlik yüzdesi

table: elmasın en geniş noktasına göre tabla genişliği

lenght: x uzunluk (mm)

width: y genişliği, mm

depth_1: z derinlik, mm

Gözlem Sayısı: 10665

Veri setinin okutulması

library(readr)
diamonds <- read_csv("C:/Users/melid/OneDrive/Masaüstü/diamonds.csv")
head(diamonds)
Veri setimizle ilgili tanımlayıcı istatistikler;

summary(diamonds)
Veri setimizdeki değişken türlerine bakalım;

str(diamonds)
Cut ve color değişkenlerinin karakter diğer değişkenlerin ise numeric olduğu görülmektedir.

Veri Setimiz de Eksik Gözlem Var Mı Diye Kontrol Edelim
library(summarytools)
print(dfSummary(diamonds, valid.col = FALSE, graph.magnif = 0.75), 
      max.tbl.height = 300, method = "render")
apply(is.na(diamonds), 2, sum)
Değişkenlere baktığımız da eksik gözlem olmadığını görmekteyiz.

Konumuz kayıp veri analizi olduğu için veri setimize eksik gözlem atayalım ve daha sonra eksik gözlem kontrolü yapalım.

Tamamen Rastgele Eksik Gözlem Veri Seti İçin EKsik Gözlem Doldurma İşlemleri
missForest kütüphanesindeki prodNA fonksiyonu ile veri setimize rastgele eksik gözlem ataması yaptık. Verimizin yaklaşık %20'si eksik gözlemlerden oluşmuştur.

library(missForest)
diamonds1<- prodNA(diamonds, noNA = 0.2)
apply(is.na(diamonds1), 2, sum)
Eksik Gözlemleri Grafikle Kontrol Edelim:

library(VIM)
aggr(diamonds1, col=c(1:2), numbers=TRUE, sortVars=TRUE, labels=names(diamonds1), cex.axis=.7, gap=3, ylab=c("Eksiklik Oranı","Eksiklik Dilimleri"))
Grafikte de verisetimiz deki değişkenlerin hepsinde yaklaşık olarak %20'si eksik(kayıp) gözlem olduğu görülmektedir.

library(Amelia)
missmap(diamonds1, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)
Amelia kütüphanesini kullanarak eksik gözlemlerimizi grafiğini çizdik verimizin yaklaşık %20'si kayıp veri olduğu gözükmektedir.

library(naniar)
gg_miss_case(diamonds1)
library(ggplot2)
ggplot(diamonds1,
       aes(x = carat,
           y = depth)) +
 geom_miss_point() + 
 theme_dark()
carat değişkenine karşılık depth değişkenin grafiği çizdirildi kırmızı noktalar eksik gözlemlerimizi ifade ederken mavi noktalar ise eksik olmayan gözlemlerimizi ifade eder.

MNAR Kategorik Değişkenler İçin Eksik Gözlemlerin Rastgele Eksik Olması
Kategorik Değişkenleri Moda Göre Doldurma
Kategorik değişkenleri mod'a göre doldurmak için ilk önce değişkendeki kategorik değerler belirlenir. Daha sonra en çok tekrar eden gözlem belirlenir.

val <- unique(diamonds1$cut[!is.na(diamonds)]) 
val
mode <- val[which.max(tabulate(match(diamonds, val)))] 
mode
cut değişkeni için en çok tekrar eden gözlem "Ideal" olduğu tespit edilir. Eksik gözlemlere "Ideal" atanır ve böylece veri setimiz mod değerine göre imputed edilir.

vec_imp <- diamonds1$cut                                 # Replicate vec_miss
vec_imp[is.na(vec_imp)] <- mode  
head(vec_imp,30)
Kategorik Değişkenleride Eksik Gözlemleri Hot-Deck ile doldurma
Hot – Deck eksik veriyi doldurma işlemini yaparken K- en yakın komşuluğu ile eksik gözlemleri doldurur. Eksik veri için ona en benzer olduğuna inanılan gözlem değeri atanır.

library(VIM)
knn_import1<- hotdeck(diamonds1, variable = c("cut","color","clarity"))
head(knn_import1,20)
Ortalama ve Medyan Göre Eksik Gözlemleri Doldurma
Ortalama ile Kayıp Gözlemleri Dolurma

Eksik gözlemleri ortalama ve medyana göre doldurma işlemi kayıp gözlem tedavi etmenin kaba bir yoludur. Gözlem sayısı düşük olan verilerde kullanıldığında yararlı olabilir. Aksi halde çok değişkenli veri setinde  hata oranını arttırır.

library(Hmisc)
ortalama_imp<-impute(diamonds1$carat, mean)  
diamonds1$carat[is.na(diamonds1$carat)] <- ortalama_imp 
Eksik gözlem olmayan veri seti ile eksik gözlemleri ortalama ile doldurmuş veri setinin ortlaması, varyansı ve standart sapması karşılaştırılmıştır.

data.frame(actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat),var(diamonds$carat)) 
           , mean_imputed_carat_mean = c(mean(ortalama_imp), sd(ortalama_imp),var(ortalama_imp))
           , row.names = c("mean", "sd","varyans"))
Ortalama ile doldurulmuş verinin ortalama, varyans ve standart sapma değeri düştüğü görülmüştür.

par(mfrow=c(1,2))
plot(density(diamonds$carat), main = "Actual caret", type="l", col="red")
plot(density(ortalama_imp), main = "Imputed caret", type="l", col="red")
yoğunluk grafiğine baktığımız da dağılımların farklı olduğu görülmektedir.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$carat, y = ortalama_imp, conf.level = 0.95)
p-value > 0,05 olduğundan H0 reddedilmez. Yani ortalam ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark yoktur.

Medyan ile Kayıp Gozlemleri Doldurma

Eksik gözlemleri medyan değerlerini atayarak medyana göre eksik gözlemlerimizi doldurduk.

medyan_imp<-impute(diamonds1$carat, median)  

diamonds1$price[is.na(diamonds1$carat)] <- medyan_imp
data.frame(actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat),var(diamonds$carat)) 
           , medyan_imputed_carat_mean = c(mean(medyan_imp), sd(medyan_imp),var(medyan_imp))
           , row.names = c("mean", "sd","varyans"))
Medyana göre doldurudğumuz caret değişkeni için ortalama, standart sapma ve varyans değerlerini verinin orjinal hali ile yani eksik gözlem olmayan haliyle karşılaştırdık. Medyana göre doldurduğumuz verinin ortalamasında, standart spmasında ve varyasında zalma olduğu görülmektedir.

par(mfrow=c(1,2))
plot(density(diamonds$carat), main = "Actual price", type="l", col="red")
plot(density(medyan_imp), main = "Imputed price", type="l", col="red")
Medyana göre imputed ettiğimiz veri setimizi ile orjinal veri setimizin yoğunluk grafiklerini karşılaştırdık ve dağılımların farklı olduğu sonucuna vardık.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$carat, y = medyan_imp, conf.level = 0.95)
p-value < 0,05 olduğundan H0 reddedilir. Yani medyan ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark vardır.

Çoklu Doldurma
Eksik değerleri doldurmak için mice kütüphanesini kullanacağız.

m=5 komutu ise çarpık veri kümelerini ifade eder. 5 varsayılan değerdir. Yani orjinal veri setimize benzer 5 farklı küme oluşturur.

Maxit komutu eksik değerleri imha ederken kullanılan iterasyon sayısıdır. Varsayılan değer 50 olduğu için bizde 50'yi seçtik.

Method komutu ise eksik gözlemleri doldururken kullanılan yöntemi ifade eder. "pmm" yöntemi ise veri setindeki eksik gözlemleri tahmini ortalamaya göre doldurur.

library(mice)

imputed_Data = mice(diamonds1, 
                    m=5, 
                    maxit = 50, 
                    method = 'pmm', 
                    seed = 50, 
                    printFlag =FALSE)

Eksik gözlemleri tahmini ortalamaya göre doldurduğumuz imputed_Data verisini özetleyelim;

summary(imputed_Data)
Eksik gözlem olan değişkenlerimize en yakın ortalama değerleri atanmış 5 farklı çarpık kümelerimiz orjinal veri setimize en yakın kümeyi bulmak için grafiklerden yararlanacağız.

head(imputed_Data$imp$price)
cut, color ve clarity değişkenleri kategorik olduğu için onlara imputed işlemi yapılmadı ve kategorik olduğu için grafiği oluşmadı. Diğer değişkenlere baktığımız da ise mavi ile gösterilen orijinal veri setimiz kırmızı ile gösterilen 5 farklı çarpık kümelerimize aittir. Gözlem sayısı çok olduğundan grafik çok anlaşılır değildir.

densityplot(imputed_Data)
Her değişken için ortalama ve standart sapma değerleridir.

plot(imputed_Data)
imputed_Data verisine 1. küme için doldurma işlemi yaptık.

imputed_Data = mice::complete(imputed_Data,1)
Eksik gözlem olmayan diamonds verisi ile imputede edilmiş veri setindeki değişkenlerin ortalama, standart sapma ve varyans değerleri karşılaştıralım.

data.frame(actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat),var(diamonds$carat)) 
           , imputed_carat_mean = c(mean(imputed_Data$carat), sd(imputed_Data$carat),var(imputed_Data$carat))
           , actual_depth_mean = c(mean(diamonds$depth), sd(diamonds$depth),var(diamonds$depth)) 
           , imputed_depth_mean = c(mean(imputed_Data$depth), sd(imputed_Data$depth),var(imputed_Data$depth))
           , actual_table_mean = c(mean(diamonds$table), sd(diamonds$table),var(diamonds$table)) 
           , imputed_table_mean = c(mean(imputed_Data$table), sd(imputed_Data$table),var(diamonds$table))
           , actual_price_mean = c(mean(diamonds$price), sd(diamonds$price),var(diamonds$price)) 
           , imputed_price_mean = c(mean(imputed_Data$price), sd(imputed_Data$price),var(imputed_Data$price))
           , actual_lenght_mean = c(mean(diamonds$lenght), sd(diamonds$lenght),var(diamonds$lenght)) 
           , imputed_lenght_mean = c(mean(imputed_Data$lenght), sd(imputed_Data$lenght),var(imputed_Data$lenght))
           , actual_width_mean = c(mean(diamonds$width), sd(diamonds$width),var(diamonds$width)) 
           , imputed_width_mean = c(mean(imputed_Data$width), sd(imputed_Data$width),var(imputed_Data$width))
           , actual_depth_1_mean = c(mean(diamonds$depth_1), sd(diamonds$depth_1),var(diamonds$depth_1)) 
           , imputed_depth_1_mean = c(mean(imputed_Data$depth_1), sd(imputed_Data$depth_1),var(imputed_Data$depth_1))
           , row.names = c("mean", "sd","varyans"))
carat değişkeni için ortalama, standart sapma ve varyans değerleir düşerken, depth değişkeni için ortalama aynı kalırken standart sapma 0,03 artmış varyans ise 0,1 oranında artmıştır. Diğer değişkenler de yaklaşık olarak böyledir.

Orijanl veri ile çoklu atama yöntemiyle tahmin edilen verilerin yoğunluk grafiklerini karşılaştırdık.

par(mfrow=c(2,2))
plot(density(diamonds$carat), main = "Actual caret", type="l", col="red")
plot(density(imputed_Data$carat), main = "Imputed caret", type="l", col="red")
plot(density(diamonds$depth), main = "Actual depth", type="l", col="red")
plot(density(imputed_Data$depth), main = "Imputed depth", type="l", col="red")
carat değişkeni için dağılımlar farklı, depth değişkeni için ise dağılımlar aynı çıkmıştır.

par(mfrow=c(2,2))
plot(density(diamonds$table), main = "Actual table", type="l", col="red")
plot(density(imputed_Data$table), main = "Imputed table", type="l", col="red")
plot(density(diamonds$price), main = "Actual price", type="l", col="red")
plot(density(imputed_Data$price), main = "Imputed price", type="l", col="red")
table değişkeni için dağılımların aynı, price değişkeni için dağılımların farklı çıktığı gözükmektedir.

par(mfrow=c(3,2))
plot(density(diamonds$lenght), main = "Actual lenght", type="l", col="red")
plot(density(imputed_Data$lenght), main = "Imputed lenght", type="l", col="red")
plot(density(diamonds$width), main = "Actual width", type="l", col="red")
plot(density(imputed_Data$width), main = "Imputed width", type="l", col="red")
plot(density(diamonds$depth_1), main = "Actual depth_1", type="l", col="red")
plot(density(imputed_Data$depth_1), main = "Imputed depth_1", type="l", col="red")
lenght, width ve depth_1 değişkenleri için ise dağılımların aynı çıktığı gyoğunluğu grafiğinde gözükmektedir.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$price, y = imputed_Data$price, conf.level = 0.95)
p-value > 0,05 olduğundan H0 reddedilmez. Yani çoklu atma yöntemi ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark yoktur.

KNN ile Eksik Gözlemleri Doldurma
KNN imputasyonunun daha basit terimlerle yaptığı şu şekildedir: Öngörülecek her gözlem için, öklid mesafesine göre 'k' en yakın gözlemlerini tanımlar ve bu 'k' mesafeye göre ağırlıklı hesaplar.

library(VIM)
knn_import<- kNN(diamonds1, variable = c("carat", "depth", "table","price","lenght","width","depth_1"), k = 5)
Eksik gözlem olmayan diamonds verisi ile kNN ile imputede edilmiş veri setindeki değişkenlerin ortalama ve standart sapmalarını karşılaştıralım.

data.frame(actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat), var(diamonds$carat)) 
           , imputed_knn_carat_mean = c(mean(knn_import$carat), sd(knn_import$carat), var(knn_import$carat))
           , actual_depth_mean = c(mean(diamonds$depth), sd(diamonds$depth),var(diamonds$depth)) 
           , imputed_knn_depth_mean = c(mean(knn_import$depth), sd(knn_import$depth),var(knn_import$depth))
           , actual_table_mean = c(mean(diamonds$table), sd(diamonds$table),var(diamonds$table)) 
           , imputed_knn_table_mean = c(mean(knn_import$table), sd(knn_import$table),var(knn_import$table))
           , actual_price_mean = c(mean(diamonds$price), sd(diamonds$price), var(diamonds$price)) 
           , imputed_knn_price_mean = c(mean(knn_import$price), sd(knn_import$price),var(knn_import$price))
           , actual_lenght_mean = c(mean(diamonds$lenght), sd(diamonds$lenght),var(diamonds$lenght)) 
           , imputed_knn_lenght_mean = c(mean(knn_import$lenght), sd(knn_import$lenght),var(knn_import$lenght))
           , actual_width_mean = c(mean(diamonds$width), sd(diamonds$width),var(diamonds$width)) 
           , imputed_knn_width_mean = c(mean(knn_import$width), sd(knn_import$width),var(knn_import$width))
           , actual_depth_1_mean = c(mean(diamonds$depth_1), sd(diamonds$depth_1),var(diamonds$depth_1)) 
           , imputed_knn_depth_1_mean = c(mean(knn_import$depth_1), sd(knn_import$depth_1),var(knn_import$depth_1))
           , row.names = c("mean", "sd","varyans"))
Orijanl veri ile KNN yöntemiyle tahmin edilen verilerin yoğunluk grafiklerini karşılaştırdık.

par(mfrow=c(2,2))
plot(density(diamonds$carat), main = "Actual caret", type="l", col="red")
plot(density(knn_import$carat), main = "Imputed caret", type="l", col="red")
plot(density(diamonds$depth), main = "Actual depth", type="l", col="red")
plot(density(knn_import$depth), main = "Imputed depth", type="l", col="red")

Orijinal veri deki carat değişkeni ile KNN yöntemi ile imputed edilen verideki carat değişkenin dağılımları farklı çıkmıştır. Orijinal veri deki depth değişkeni ile KNN yöntemi ile imputed edilen verideki depth değişkenin dağılımları aynı çıkmıştır.

par(mfrow=c(2,2))
plot(density(diamonds$table), main = "Actual table", type="l", col="red")
plot(density(knn_import$table), main = "Imputed table", type="l", col="red")
plot(density(diamonds$price), main = "Actual price", type="l", col="red")
plot(density(knn_import$price), main = "Imputed price", type="l", col="red")
Orijinal veri deki table değişkeni ile KNN yöntemi ile imputed edilen verideki table değişkenin dağılımları aynı çıkmıştır. Orijinal veri deki price değişkeni ile KNN yöntemi ile imputed edilen verideki price değişkenin dağılımları çok yakın çıkmıştır.

par(mfrow=c(3,2))
plot(density(diamonds$lenght), main = "Actual lenght", type="l", col="red")
plot(density(knn_import$lenght), main = "Imputed lenght", type="l", col="red")
plot(density(diamonds$width), main = "Actual width", type="l", col="red")
plot(density(knn_import$width), main = "Imputed width", type="l", col="red")
plot(density(diamonds$depth_1), main = "Actual depth_1", type="l", col="red")
plot(density(knn_import$depth_1), main = "Imputed depth_1", type="l", col="red")
lenght, width ve depth_1 değişkenleri için dağılımlar aynı çıkmıştır.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$price, y =knn_import$price, conf.level = 0.95)
p-value > 0,05 olduğundan H0 reddedilmez. Yani k en yakın komşuluk ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark yoktur.

Hot-Deck(kNN) ile doldurma
Hot – Deck eksik veriyi doldurma işlemini yaparken K- en yakın komşuluğu ile eksik gözlemleri doldurur. Eksik veri için ona en benzer olduğuna inanılan gözlem değeri atanır.

library(VIM)
hot.deck_import<-hotdeck(diamonds1,variable = c("lenght","width","depth","depth_1","table","price"))
head(hot.deck_import)
Hot deck yöntemi ile imputed edilen veri ile orjinal veri setimizin ortalama, standart sapma ve varyans değerlerini karşılaştıralım.

data.frame(actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat), var(diamonds$carat)) 
           , imputed_hot_carat_mean = c(mean(hot.deck_import$carat), sd(hot.deck_import$carat), var(hot.deck_import$carat))
           , actual_depth_mean = c(mean(diamonds$depth), sd(diamonds$depth),var(diamonds$depth)) 
           , imputed_hot_depth_mean = c(mean(hot.deck_import$depth), sd(hot.deck_import$depth),var(hot.deck_import$depth))
           , actual_table_mean = c(mean(diamonds$table), sd(diamonds$table),var(diamonds$table)) 
           , imputed_hot_table_mean = c(mean(hot.deck_import$table), sd(hot.deck_import$table),var(hot.deck_import$table))
           , actual_price_mean = c(mean(diamonds$price), sd(diamonds$price), var(diamonds$price)) 
           , imputed_hot_price_mean = c(mean(hot.deck_import$price), sd(hot.deck_import$price),var(hot.deck_import$price))
           , actual_lenght_mean = c(mean(diamonds$lenght), sd(diamonds$lenght),var(diamonds$lenght)) 
           , imputed_hot_lenght_mean = c(mean(hot.deck_import$lenght), sd(hot.deck_import$lenght),
                                         var(hot.deck_import$lenght))
           , actual_width_mean = c(mean(diamonds$width), sd(diamonds$width),var(diamonds$width)) 
           , imputed_hot_width_mean = c(mean(hot.deck_import$width), sd(hot.deck_import$width),var(hot.deck_import$width))
           , actual_depth_1_mean = c(mean(diamonds$depth_1), sd(diamonds$depth_1),var(diamonds$depth_1)) 
           , imputed_hot_depth_1_mean = c(mean(hot.deck_import$depth_1), sd(hot.deck_import$depth_1),
                                          var(hot.deck_import$depth_1))
           , row.names = c("mean", "sd","varyans"))
Hot deck yöntemi ile imputed edilen veri ile orjinal veri setimizin yoğunluk grafikleri ile dağılımlarını karşılaştırılması;

par(mfrow=c(2,2))
plot(density(diamonds$carat), main = "Actual caret", type="l", col="red")
plot(density(hot.deck_import$carat), main = "Imputed caret", type="l", col="red")
plot(density(diamonds$depth), main = "Actual depth", type="l", col="red")
plot(density(hot.deck_import$depth), main = "Imputed depth", type="l", col="red")
Orijinal veri deki caret değişkeni ile hot deck yöntemi ile imputed edilen verideki caret değişkenin dağılımları aynı çıkmıştır. Orijinal veri deki depth değişkeni ile hot deck yöntemi ile imputed edilen verideki depth değişkenin dağılımları aynı çıkmıştır.

par(mfrow=c(2,2))
plot(density(diamonds$table), main = "Actual table", type="l", col="red")
plot(density(hot.deck_import$table), main = "Imputed table", type="l", col="red")
plot(density(diamonds$price), main = "Actual price", type="l", col="red")
plot(density(hot.deck_import$price), main = "Imputed price", type="l", col="red")
Orijinal veri deki table değişkeni ile hot deck yöntemi ile imputed edilen verideki table değişkenin dağılımları aynı çıkmıştır. Orijinal veri deki price değişkeni ile hot deck yöntemi ile imputed edilen verideki price değişkenin dağılımları aynı çıkmıştır.

par(mfrow=c(3,2))
plot(density(diamonds$lenght), main = "Actual lenght", type="l", col="red")
plot(density(hot.deck_import$lenght), main = "Imputed lenght", type="l", col="red")
plot(density(diamonds$width), main = "Actual width", type="l", col="red")
plot(density(hot.deck_import$width), main = "Imputed width", type="l", col="red")
plot(density(diamonds$depth_1), main = "Actual depth_1", type="l", col="red")
plot(density(hot.deck_import$depth_1), main = "Imputed depth_1", type="l", col="red")
Orijinal veri deki lenght, width ve depth_1 değişkenleri ile hot deck yöntemi ile imputed edilen verideki lenght, width ve depth_1 değişkenleri dağılımları aynı çıkmıştır.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$price, y = hot.deck_import$price, conf.level = 0.95)
p-value > 0,05 olduğundan H0 reddedilmez. Yani hot deck yöntemi ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark yoktur.

Random Forest Algoritması ile Eksik Gözlem Tamamlama
Random forest yaklaşımı ile eksik gözlemleri imputed etmek için mice kütüphanesini kullanmamız yeterlidir. Bunun dışında missForest kütüphanesi de karar ağacı algoritmması üzerinde kurulmuş bir kütüphanedir. Ancak biz mice kütüphanesini tercih ettik. mice fonksiyonu içinde yer alan method komutunu "rf" seçtiğimiz de random forest algoritması ile imputede işlemi yapar.

library(mice)
miceMod <- mice(diamonds1[, !names(diamonds1) %in% "price"], method="rf")  
miceOutput <- complete(miceMod)  
anyNA(miceOutput)
Random forest algoritması ile imputed edilmiş veri ile orjinal verimizin ortalama, standart sapma ve varyans değerlerini karşılaştıralım.

data.frame(  actual_depth_mean = c(mean(diamonds$depth), sd(diamonds$depth),var(diamonds$depth)) 
           , imputed_rf_depth_mean = c(mean(miceOutput$depth), sd(miceOutput$depth),var(miceOutput$depth))
           , actual_table_mean = c(mean(diamonds$table), sd(diamonds$table),var(diamonds$table)) 
           , imputed_rf_table_mean = c(mean(miceOutput$table), sd(miceOutput$table),var(miceOutput$table))
           , actual_price_mean = c(mean(diamonds$carat), sd(diamonds$carat), var(diamonds$carat)) 
           , imputed_rf_price_mean = c(mean(miceOutput$carat), sd(miceOutput$carat),var(miceOutput$carat))
           , actual_lenght_mean = c(mean(diamonds$lenght), sd(diamonds$lenght),var(diamonds$lenght)) 
           , imputed_rf_lenght_mean = c(mean(miceOutput$lenght), sd(miceOutput$lenght),
                                         var(miceOutput$lenght))
           , actual_width_mean = c(mean(diamonds$width), sd(diamonds$width),var(diamonds$width)) 
           , imputed_rf_width_mean = c(mean(miceOutput$width), sd(miceOutput$width),var(miceOutput$width))
           , actual_depth_1_mean = c(mean(diamonds$depth_1), sd(diamonds$depth_1),var(diamonds$depth_1)) 
           , imputed_rf_depth_1_mean = c(mean(miceOutput$depth_1), sd(miceOutput$depth_1),
                                          var(miceOutput$depth_1))
           , row.names = c("mean", "sd","varyans"))
Random forest algoritması ile imputed edilmiş veri ile orjinal verimizin yoğunluk grafikleri ile dağılımlarını karşılaştıralım.

par(mfrow=c(3,2))
plot(density(diamonds$depth), main = "Actual depth", type="l", col="red")
plot(density(miceOutput$depth), main = "Imputed depth", type="l", col="red")
plot(density(diamonds$table), main = "Actual table", type="l", col="red")
plot(density(miceOutput$table), main = "Imputed table", type="l", col="red")
plot(density(diamonds$carat), main = "Actual price", type="l", col="red")
plot(density(miceOutput$carat), main = "Imputed price", type="l", col="red")
Orijinal veri deki depth ve table değişkenleri ile random forest yöntemi ile imputed edilen verideki depth ve table değişkenleri dağılımları aynı çıkmıştır. Ancak price değişknein dağılımı random forest yöntemi ile imputed edilen verideki price değişkeninden farklıdır.

par(mfrow=c(3,2))
plot(density(diamonds$lenght), main = "Actual lenght", type="l", col="red")
plot(density(miceOutput$lenght), main = "Imputed lenght", type="l", col="red")
plot(density(diamonds$width), main = "Actual width", type="l", col="red")
plot(density(miceOutput$width), main = "Imputed width", type="l", col="red")
plot(density(diamonds$depth_1), main = "Actual depth_1", type="l", col="red")
plot(density(miceOutput$depth_1), main = "Imputed depth_1", type="l", col="red")
Orijinal veri deki lenght, width ve depth_1 değişkenleri ile random forest yöntemi ile imputed edilen verideki lenght, width ve depth_1 değişkenleri dağılımları aynı çıkmıştır.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$carat, y = miceOutput$carat, conf.level = 0.95)
p-value < 0,05 olduğundan H0 reddedilir. Yani random forest ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark vardır.

Doğrusal Regresyon Modeli ile Eksik Gözlemleri Doldurma
mice kütüphanesin de yer alan mice fonksiyonu içinde yer alan method komutunu "norm.boot" seçtiğimiz de norm.boot methodu bootstrap kullanarak doğrusal regersyon yapar.

data <- diamonds1[, c("carat", "price","table","lenght","depth","width","depth_1")]
imp <- mice(data, method = "norm.boot", m = 1, maxit = 1,
            seed = 1, print = FALSE)
imp_Data = mice::complete(imp) # veri seti haline getiren komut
Doğrusal regresyon algoritması ile imputed edilmiş veri ile orjinal verimizin ortalama, standart sapma ve varyans değerlerini karşılaştıralım.

data.frame(  actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat),var(diamonds$carat)) 
           , imputed_carat_mean = c(mean(imp_Data$carat), sd(imp_Data$carat),var(imp_Data$carat))
           , actual_price_mean = c(mean(diamonds$price), sd(diamonds$price),var(diamonds$price)) 
           , imputed_price_mean = c(mean(imp_Data$price), sd(imp_Data$price),var(imp_Data$price))
           , actual_table_mean = c(mean(diamonds$table), sd(diamonds$table),var(diamonds$table)) 
           , imputed_table_mean = c(mean(imp_Data$table), sd(imp_Data$table),var(imp_Data$table))
           , actual_lenght_mean = c(mean(diamonds$lenght), sd(diamonds$lenght),var(diamonds$lenght)) 
           , imputed_lenght_mean = c(mean(imp_Data$lenght), sd(imp_Data$lenght),var(imp_Data$lenght))
           , actual_depth_mean = c(mean(diamonds$depth), sd(diamonds$depth),var(diamonds$depth)) 
           , imputed_depth_mean = c(mean(imp_Data$depth), sd(imp_Data$depth),var(imp_Data$depth))
           , actual_width_mean = c(mean(diamonds$width), sd(diamonds$width),var(diamonds$width)) 
           , imputed_width_mean = c(mean(imp_Data$width), sd(imp_Data$width),var(imp_Data$width))
           , actual_depth_1_mean = c(mean(diamonds$depth_1), sd(diamonds$depth_1),var(diamonds$depth_1)) 
           , imputed_depth_1_mean = c(mean(imp_Data$depth_1), sd(imp_Data$depth_1),var(imp_Data$depth_1))
           , row.names = c("mean", "sd","varyans"))
Doğrusal regresyon algoritması ile imputed edilmiş veri ile orjinal verimizin yoğunluk grafikleri ile dağılımlarını karşılaştıralım.

par(mfrow=c(2,2))
plot(density(diamonds$carat), main = "Actual caret", type="l", col="red")
plot(density(imp_Data$carat), main = "Imputed caret", type="l", col="red")
plot(density(diamonds$price), main = "Actual price", type="l", col="red")
plot(density(imp_Data$price), main = "Imputed price", type="l", col="red")
Orijinal veri deki carat ve price değişkenleri ile doğrusal regresyon yöntemi ile imputed edilen verideki carat ve price değişkenleri dağılımları farklı çıkmıştır.

par(mfrow=c(3,2))
plot(density(diamonds$table), main = "Actual table", type="l", col="red")
plot(density(imp_Data$table), main = "Imputed table", type="l", col="red")
plot(density(diamonds$lenght), main = "Actual length", type="l", col="red")
plot(density(imp_Data$lenght), main = "Imputed length", type="l", col="red")
plot(density(diamonds$depth), main = "Actual depth", type="l", col="red")
plot(density(imp_Data$depth), main = "Imputed depth", type="l", col="red")
Orijinal veri deki table, lenght ve depth değişkenleri ile regresyon yöntemi ile imputed edilen verideki table, lenght ve depth değişkenleri dağılımları aynı çıkmıştır.

par(mfrow=c(2,2))
plot(density(diamonds$width), main = "Actual width", type="l", col="red")
plot(density(imp_Data$width), main = "Imputed width", type="l", col="red")
plot(density(diamonds$depth_1), main = "Actual depth_1", type="l", col="red")
plot(density(imp_Data$depth_1), main = "Imputed depth_1", type="l", col="red")
Orijinal veri deki width ve depth_ değişkenleri ile regresyon yöntemi ile imputed edilen verideki width ve depth_1 değişkenleri dağılımları aynı çıkmıştır.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$price, y =imp_Data$price, conf.level = 0.95)
p-value > 0,05 olduğundan H0 reddedilmez. Yani doğrusal regresyon ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark yoktur.

MAR Rastgele Eksik Veri Setleri için Doldurma Yöntemleri
diamonds2 = diamonds
for(i in 200:700){
  diamonds2[i,] = NA
}
apply(is.na(diamonds2), 2, sum)
Değişkenlerimizin hepsinde 501 gözlemin eksik olduğu görülür. Bu durumu grafikle inceleyelim.

library(Amelia)
missmap(diamonds2, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)
Veri setimizin yaklaşık % %'i eksik gözlem den oluşmaktadır. Bütün değişkenlerin eksik gözlem olması gerçek hayatta karşılacağımız bir durum değildir. Ancak biz ödevimiz kayıp veri analizi olduğundan eksik gözlem ataması yaptık.

Ortalama ile Kayıp Gözlemleri Dolurma

Eksik gözlemleri ortalama ve medyana göre doldurma işlemi kayıp gözlem tedavi etmenin kaba bir yoludur. Gözlem sayısı düşük olan verilerde kullanıldığında yararlı olabilir. Aksi halde çok değişkenli veri setinde  hata oranını arttırır.

library(Hmisc)
ortalama_imp1<-impute(diamonds2$price, mean)  
diamonds2$carat[is.na(diamonds2$carat)] <- ortalama_imp1
Eksik gözlem olmayan veri seti ile eksik gözlemleri ortalama ile doldurmuş veri setinin ortlaması, varyansı ve standart sapması karşılaştırılmıştır.

data.frame(actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat),var(diamonds$carat)) 
           , mean_imputed_carat_mean = c(mean(ortalama_imp), sd(ortalama_imp),var(ortalama_imp))
           , row.names = c("mean", "sd","varyans"))
Ortalama ile doldurulmuş verinin ortalama, varyans ve standart sapma değeri düştüğü görülmüştür.

par(mfrow=c(1,2))
plot(density(diamonds$carat), main = "Actual caret", type="l", col="red")
plot(density(ortalama_imp), main = "Imputed caret", type="l", col="red")
yoğunluk grafiğine baktığımız da dağılımların farklı olduğu görülmektedir.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$carat, y = ortalama_imp, conf.level = 0.95)
p-value > 0,05 olduğundan H0 reddedilmez. Yani ortalam ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark yoktur.

Medyan ile Kayıp Gozlemleri Doldurma

Eksik gözlemleri medyan değerlerini atayarak medyana göre eksik gözlemlerimizi doldurduk.

medyan_imp<-impute(diamonds2$carat, median)  

diamonds1$price[is.na(diamonds2$carat)] <- medyan_imp
data.frame(actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat),var(diamonds$carat)) 
           , medyan_imputed_carat_mean = c(mean(medyan_imp), sd(medyan_imp),var(medyan_imp))
           , row.names = c("mean", "sd","varyans"))
Medyana göre doldurudğumuz caret değişkeni için ortalama, standart sapma ve varyans değerlerini verinin orjinal hali ile yani eksik gözlem olmayan haliyle karşılaştırdık. Medyana göre doldurduğumuz verinin ortalamasında, standart spmasında ve varyasında zalma olduğu görülmektedir.

par(mfrow=c(1,2))
plot(density(diamonds$carat), main = "Actual price", type="l", col="red")
plot(density(medyan_imp), main = "Imputed price", type="l", col="red")
Medyana göre imputed ettiğimiz veri setimizi ile orjinal veri setimizin yoğunluk grafiklerini karşılaştırdık ve dağılımların farklı olduğu sonucuna vardık.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$carat, y = medyan_imp, conf.level = 0.95)
p-value < 0,05 olduğundan H0 reddedilir. Yani medyan ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark vardır.

Çoklu Doldurma
Eksik değerleri doldurmak için mice kütüphanesini kullanacağız.

m=5 komutu ise çarpık veri kümelerini ifade eder. 5 varsayılan değerdir. Yani orjinal veri setimize benzer 5 farklı küme oluşturur.

Maxit komutu eksik değerleri imha ederken kullanılan iterasyon sayısıdır. Varsayılan değer 50 olduğu için bizde 50'yi seçtik.

Method komutu ise eksik gözlemleri doldururken kullanılan yöntemi ifade eder. "pmm" yöntemi ise veri setindeki eksik gözlemleri tahmini ortalamaya göre doldurur.

library(mice)

imputed_Data1 = mice(diamonds2, 
                    m=5, 
                    maxit = 50, 
                    method = 'pmm', 
                    seed = 50, 
                    printFlag =FALSE)

Eksik gözlemleri tahmini ortalamaya göre doldurduğumuz imputed_Data1 verisini özetleyelim;

summary(imputed_Data1)
Eksik gözlem olan değişkenlerimize en yakın ortalama değerleri atanmış 5 farklı çarpık kümelerimiz orjinal veri setimize en yakın kümeyi bulmak için grafiklerden yararlanacağız.

head(imputed_Data1$imp$price)
cut, color ve clarity değişkenleri kategorik olduğu için onlara imputed işlemi yapılmadı ve kategorik olduğu için grafiği oluşmadı. Diğer değişkenlere baktığımız da ise mavi ile gösterilen orijinal veri setimiz kırmızı ile gösterilen 5 farklı çarpık kümelerimize aittir. Gözlem sayısı çok olduğundan grafik çok anlaşılır değildir.

densityplot(imputed_Data1)
Her değişken için ortalama ve standart sapma değerleridir.

plot(imputed_Data1)
imputed_Data1 verisine 1. küme için doldurma işlemi yaptık.

imputed_Data1 = mice::complete(imputed_Data1,1)
Eksik gözlem olmayan diamonds verisi ile imputede edilmiş veri setindeki değişkenlerin ortalama, standart sapma ve varyans değerleri karşılaştıralım.

data.frame(actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat),var(diamonds$carat)) 
           , imputed_carat_mean = c(mean(imputed_Data1$carat), sd(imputed_Data1$carat),var(imputed_Data1$carat))
           , actual_depth_mean = c(mean(diamonds$depth), sd(diamonds$depth),var(diamonds$depth)) 
           , imputed_depth_mean = c(mean(imputed_Data1$depth), sd(imputed_Data1$depth),var(imputed_Data1$depth))
           , actual_table_mean = c(mean(diamonds$table), sd(diamonds$table),var(diamonds$table)) 
           , imputed_table_mean = c(mean(imputed_Data1$table), sd(imputed_Data1$table),var(imputed_Data1$table))
           , actual_price_mean = c(mean(diamonds$price), sd(diamonds$price),var(diamonds$price)) 
           , imputed_price_mean = c(mean(imputed_Data1$price), sd(imputed_Data1$price),var(imputed_Data1$price))
           , actual_lenght_mean = c(mean(diamonds$lenght), sd(diamonds$lenght),var(diamonds$lenght)) 
           , imputed_lenght_mean = c(mean(imputed_Data1$lenght), sd(imputed_Data1$lenght),var(imputed_Data1$lenght))
           , actual_width_mean = c(mean(diamonds$width), sd(diamonds$width),var(diamonds$width)) 
           , imputed_width_mean = c(mean(imputed_Data1$width), sd(imputed_Data1$width),var(imputed_Data1$width))
           , actual_depth_1_mean = c(mean(diamonds$depth_1), sd(diamonds$depth_1),var(diamonds$depth_1)) 
           , imputed_depth_1_mean = c(mean(imputed_Data1$depth_1), sd(imputed_Data1$depth_1),var(imputed_Data1$depth_1))
           , row.names = c("mean", "sd","varyans"))
carat değişkeni için ortalama, standart sapma ve varyans değerleir düşerken, depth değişkeni için ortalama aynı kalırken standart sapma 0,03 artmış varyans ise 0,1 oranında artmıştır. Diğer değişkenler de yaklaşık olarak böyledir.

Orijanl veri ile çoklu atama yöntemiyle tahmin edilen verilerin yoğunluk grafiklerini karşılaştırdık.

par(mfrow=c(2,2))
plot(density(diamonds$carat), main = "Actual caret", type="l", col="red")
plot(density(imputed_Data1$carat), main = "Imputed caret", type="l", col="red")
plot(density(diamonds$depth), main = "Actual depth", type="l", col="red")
plot(density(imputed_Data1$depth), main = "Imputed depth", type="l", col="red")
carat değişkeni için dağılımlar farklı, depth değişkeni için ise dağılımlar aynı çıkmıştır.

par(mfrow=c(2,2))
plot(density(diamonds$table), main = "Actual table", type="l", col="red")
plot(density(imputed_Data1$table), main = "Imputed table", type="l", col="red")
plot(density(diamonds$price), main = "Actual price", type="l", col="red")
plot(density(imputed_Data1$price), main = "Imputed price", type="l", col="red")
table değişkeni için dağılımların aynı, price değişkeni için dağılımların farklı çıktığı gözükmektedir.

par(mfrow=c(3,2))
plot(density(diamonds$lenght), main = "Actual lenght", type="l", col="red")
plot(density(imputed_Data1$lenght), main = "Imputed lenght", type="l", col="red")
plot(density(diamonds$width), main = "Actual width", type="l", col="red")
plot(density(imputed_Data1$width), main = "Imputed width", type="l", col="red")
plot(density(diamonds$depth_1), main = "Actual depth_1", type="l", col="red")
plot(density(imputed_Data1$depth_1), main = "Imputed depth_1", type="l", col="red")
lenght, width ve depth_1 değişkenleri için ise dağılımların aynı çıktığı gyoğunluğu grafiğinde gözükmektedir.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$price, y = imputed_Data1$price, conf.level = 0.95)
p-value < 0,05 olduğundan H0 reddedilir. Yani çoklu atma yöntemi ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark vardır.

KNN ile Eksik Gözlemleri Doldurma
KNN imputasyonunun daha basit terimlerle yaptığı şu şekildedir: Öngörülecek her gözlem için, öklid mesafesine göre 'k' en yakın gözlemlerini tanımlar ve bu 'k' mesafeye göre ağırlıklı hesaplar.

library(VIM)
knn_import1<- kNN(diamonds2, variable = c("carat", "depth", "table","price","lenght","width","depth_1"), k = 5)
Eksik gözlem olmayan diamonds verisi ile kNN ile imputede edilmiş veri setindeki değişkenlerin ortalama ve standart sapmalarını karşılaştıralım.

data.frame(actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat), var(diamonds$carat)) 
           , imputed_knn_carat_mean = c(mean(knn_import1$carat), sd(knn_import1$carat), var(knn_import1$carat))
           , actual_depth_mean = c(mean(diamonds$depth), sd(diamonds$depth),var(diamonds$depth)) 
           , imputed_knn_depth_mean = c(mean(knn_import1$depth), sd(knn_import1$depth),var(knn_import1$depth))
           , actual_table_mean = c(mean(diamonds$table), sd(diamonds$table),var(diamonds$table)) 
           , imputed_knn_table_mean = c(mean(knn_import1$table), sd(knn_import1$table),var(knn_import1$table))
           , actual_price_mean = c(mean(diamonds$price), sd(diamonds$price), var(diamonds$price)) 
           , imputed_knn_price_mean = c(mean(knn_import1$price), sd(knn_import1$price),var(knn_import1$price))
           , actual_lenght_mean = c(mean(diamonds$lenght), sd(diamonds$lenght),var(diamonds$lenght)) 
           , imputed_knn_lenght_mean = c(mean(knn_import1$lenght), sd(knn_import1$lenght),var(knn_import1$lenght))
           , actual_width_mean = c(mean(diamonds$width), sd(diamonds$width),var(diamonds$width)) 
           , imputed_knn_width_mean = c(mean(knn_import1$width), sd(knn_import1$width),var(knn_import1$width))
           , actual_depth_1_mean = c(mean(diamonds$depth_1), sd(diamonds$depth_1),var(diamonds$depth_1)) 
           , imputed_knn_depth_1_mean = c(mean(knn_import1$depth_1), sd(knn_import1$depth_1),var(knn_import1$depth_1))
           , row.names = c("mean", "sd","varyans"))
Orijanl veri ile KNN yöntemiyle tahmin edilen verilerin yoğunluk grafiklerini karşılaştırdık.

par(mfrow=c(2,2))
plot(density(diamonds$carat), main = "Actual caret", type="l", col="red")
plot(density(knn_import1$carat), main = "Imputed caret", type="l", col="red")
plot(density(diamonds$depth), main = "Actual depth", type="l", col="red")
plot(density(knn_import1$depth), main = "Imputed depth", type="l", col="red")

Orijinal veri deki carat değişkeni ile KNN yöntemi ile imputed edilen verideki carat değişkenin dağılımları farklı çıkmıştır. Orijinal veri deki depth değişkeni ile KNN yöntemi ile imputed edilen verideki depth değişkenin dağılımları aynı çıkmıştır.

par(mfrow=c(2,2))
plot(density(diamonds$table), main = "Actual table", type="l", col="red")
plot(density(knn_import1$table), main = "Imputed table", type="l", col="red")
plot(density(diamonds$price), main = "Actual price", type="l", col="red")
plot(density(knn_import1$price), main = "Imputed price", type="l", col="red")
Orijinal veri deki table değişkeni ile KNN yöntemi ile imputed edilen verideki table değişkenin dağılımları aynı çıkmıştır. Orijinal veri deki price değişkeni ile KNN yöntemi ile imputed edilen verideki price değişkenin dağılımları çok yakın çıkmıştır.

par(mfrow=c(3,2))
plot(density(diamonds$lenght), main = "Actual lenght", type="l", col="red")
plot(density(knn_import1$lenght), main = "Imputed lenght", type="l", col="red")
plot(density(diamonds$width), main = "Actual width", type="l", col="red")
plot(density(knn_import1$width), main = "Imputed width", type="l", col="red")
plot(density(diamonds$depth_1), main = "Actual depth_1", type="l", col="red")
plot(density(knn_import1$depth_1), main = "Imputed depth_1", type="l", col="red")
lenght, width ve depth_1 değişkenleri için dağılımlar aynı çıkmıştır.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$price, y =knn_import1$price, conf.level = 0.95)
p-value < 0,05 olduğundan H0 reddedilir. Yani k en yakın komşuluk ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark vardır.

Hot-Deck(kNN) ile doldurma
Hot – Deck eksik veriyi doldurma işlemini yaparken K- en yakın komşuluğu ile eksik gözlemleri doldurur. Eksik veri için ona en benzer olduğuna inanılan gözlem değeri atanır.

library(VIM)
hot.deck_import1<-hotdeck(diamonds2,variable = c("carat","lenght","width","depth","depth_1","table","price"))
head(hot.deck_import1)
Hot deck yöntemi ile imputed edilen veri ile orjinal veri setimizin ortalama, standart sapma ve varyans değerlerini karşılaştıralım.

data.frame(actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat), var(diamonds$carat)) 
           , imputed_hot_carat_mean = c(mean(hot.deck_import1$carat), sd(hot.deck_import1$carat),
                                        var(hot.deck_import1$carat))
           , actual_depth_mean = c(mean(diamonds$depth), sd(diamonds$depth),var(diamonds$depth)) 
           , imputed_hot_depth_mean = c(mean(hot.deck_import1$depth), sd(hot.deck_import1$depth),
                                        var(hot.deck_import1$depth))
           , actual_table_mean = c(mean(diamonds$table), sd(diamonds$table),var(diamonds$table)) 
           , imputed_hot_table_mean = c(mean(hot.deck_import1$table), sd(hot.deck_import1$table),
                                        var(hot.deck_import1$table))
           , actual_price_mean = c(mean(diamonds$price), sd(diamonds$price), var(diamonds$price)) 
           , imputed_hot_price_mean = c(mean(hot.deck_import1$price), sd(hot.deck_import1$price),
                                        var(hot.deck_import1$price))
           , actual_lenght_mean = c(mean(diamonds$lenght), sd(diamonds$lenght),var(diamonds$lenght)) 
           , imputed_hot_lenght_mean = c(mean(hot.deck_import1$lenght), sd(hot.deck_import1$lenght),
                                         var(hot.deck_import1$lenght))
           , actual_width_mean = c(mean(diamonds$width), sd(diamonds$width),var(diamonds$width)) 
           , imputed_hot_width_mean = c(mean(hot.deck_import1$width), sd(hot.deck_import1$width),
                                        var(hot.deck_import1$width))
           , actual_depth_1_mean = c(mean(diamonds$depth_1), sd(diamonds$depth_1),var(diamonds$depth_1)) 
           , imputed_hot_depth_1_mean = c(mean(hot.deck_import1$depth_1), sd(hot.deck_import1$depth_1),
                                          var(hot.deck_import1$depth_1))
           , row.names = c("mean", "sd","varyans"))
Hot deck yöntemi ile imputed edilen veri ile orjinal veri setimizin yoğunluk grafikleri ile dağılımlarını karşılaştırılması;

par(mfrow=c(2,2))
plot(density(diamonds$carat), main = "Actual caret", type="l", col="red")
plot(density(hot.deck_import1$carat), main = "Imputed caret", type="l", col="red")
plot(density(diamonds$depth), main = "Actual depth", type="l", col="red")
plot(density(hot.deck_import1$depth), main = "Imputed depth", type="l", col="red")
Orijinal veri deki caret değişkeni ile hot deck yöntemi ile imputed edilen verideki caret değişkenin dağılımları aynı çıkmıştır. Orijinal veri deki depth değişkeni ile hot deck yöntemi ile imputed edilen verideki depth değişkenin dağılımları aynı çıkmıştır.

par(mfrow=c(2,2))
plot(density(diamonds$table), main = "Actual table", type="l", col="red")
plot(density(hot.deck_import1$table), main = "Imputed table", type="l", col="red")
plot(density(diamonds$price), main = "Actual price", type="l", col="red")
plot(density(hot.deck_import1$price), main = "Imputed price", type="l", col="red")
Orijinal veri deki table değişkeni ile hot deck yöntemi ile imputed edilen verideki table değişkenin dağılımları aynı çıkmıştır. Orijinal veri deki price değişkeni ile hot deck yöntemi ile imputed edilen verideki price değişkenin dağılımları aynı çıkmıştır.

par(mfrow=c(3,2))
plot(density(diamonds$lenght), main = "Actual lenght", type="l", col="red")
plot(density(hot.deck_import1$lenght), main = "Imputed lenght", type="l", col="red")
plot(density(diamonds$width), main = "Actual width", type="l", col="red")
plot(density(hot.deck_import1$width), main = "Imputed width", type="l", col="red")
plot(density(diamonds$depth_1), main = "Actual depth_1", type="l", col="red")
plot(density(hot.deck_import1$depth_1), main = "Imputed depth_1", type="l", col="red")
Orijinal veri deki lenght, width ve depth_1 değişkenleri ile hot deck yöntemi ile imputed edilen verideki lenght, width ve depth_1 değişkenleri dağılımları aynı çıkmıştır.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$price, y = hot.deck_import1$price, conf.level = 0.95)
p-value < 0,05 olduğundan H0 reddedilir. Yani hot deck yöntemi ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark vardır.

Random Forest Algoritması ile Eksik Gözlem Tamamlama
Random forest yaklaşımı ile eksik gözlemleri imputed etmek için mice kütüphanesini kullanmamız yeterlidir. Bunun dışında missForest kütüphanesi de karar ağacı algoritmması üzerinde kurulmuş bir kütüphanedir. Ancak biz mice kütüphanesini tercih ettik. mice fonksiyonu içinde yer alan method komutunu "rf" seçtiğimiz de random forest algoritması ile imputede işlemi yapar.

library(mice)
miceMod1 <- mice(diamonds2[, !names(diamonds2) %in% "price"], method="rf")  
miceOutput1 <- complete(miceMod1)  
anyNA(miceOutput1)
Random forest algoritması ile imputed edilmiş veri ile orjinal verimizin ortalama, standart sapma ve varyans değerlerini karşılaştıralım.

data.frame(  actual_depth_mean = c(mean(diamonds$depth), sd(diamonds$depth),var(diamonds$depth)) 
           , imputed_rf_depth_mean = c(mean(miceOutput1$depth), sd(miceOutput1$depth),var(miceOutput1$depth))
           , actual_table_mean = c(mean(diamonds$table), sd(diamonds$table),var(diamonds$table)) 
           , imputed_rf_table_mean = c(mean(miceOutput1$table), sd(miceOutput1$table),var(miceOutput1$table))
           , actual_price_mean = c(mean(diamonds$carat), sd(diamonds$carat), var(diamonds$carat)) 
           , imputed_rf_price_mean = c(mean(miceOutput1$carat), sd(miceOutput1$carat),var(miceOutput1$carat))
           , actual_lenght_mean = c(mean(diamonds$lenght), sd(diamonds$lenght),var(diamonds$lenght)) 
           , imputed_rf_lenght_mean = c(mean(miceOutput1$lenght), sd(miceOutput1$lenght),
                                         var(miceOutput1$lenght))
           , actual_width_mean = c(mean(diamonds$width), sd(diamonds$width),var(diamonds$width)) 
           , imputed_rf_width_mean = c(mean(miceOutput1$width), sd(miceOutput1$width),var(miceOutput1$width))
           , actual_depth_1_mean = c(mean(diamonds$depth_1), sd(diamonds$depth_1),var(diamonds$depth_1)) 
           , imputed_rf_depth_1_mean = c(mean(miceOutput1$depth_1), sd(miceOutput1$depth_1),
                                          var(miceOutput1$depth_1))
           , row.names = c("mean", "sd","varyans"))
Random forest algoritması ile imputed edilmiş veri ile orjinal verimizin yoğunluk grafikleri ile dağılımlarını karşılaştıralım.

par(mfrow=c(3,2))
plot(density(diamonds$depth), main = "Actual depth", type="l", col="red")
plot(density(miceOutput1$depth), main = "Imputed depth", type="l", col="red")
plot(density(diamonds$table), main = "Actual table", type="l", col="red")
plot(density(miceOutput1$table), main = "Imputed table", type="l", col="red")
plot(density(diamonds$carat), main = "Actual price", type="l", col="red")
plot(density(miceOutput1$carat), main = "Imputed price", type="l", col="red")
Orijinal veri deki depth ve table değişkenleri ile random forest yöntemi ile imputed edilen verideki depth ve table değişkenleri dağılımları aynı çıkmıştır. Ancak price değişknein dağılımı random forest yöntemi ile imputed edilen verideki price değişkeninden farklıdır.

par(mfrow=c(3,2))
plot(density(diamonds$lenght), main = "Actual lenght", type="l", col="red")
plot(density(miceOutput1$lenght), main = "Imputed lenght", type="l", col="red")
plot(density(diamonds$width), main = "Actual width", type="l", col="red")
plot(density(miceOutput1$width), main = "Imputed width", type="l", col="red")
plot(density(diamonds$depth_1), main = "Actual depth_1", type="l", col="red")
plot(density(miceOutput1$depth_1), main = "Imputed depth_1", type="l", col="red")
Orijinal veri deki lenght, width ve depth_1 değişkenleri ile random forest yöntemi ile imputed edilen verideki lenght, width ve depth_1 değişkenleri dağılımları aynı çıkmıştır.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$carat, y = miceOutput1$carat, conf.level = 0.95)
p-value < 0,05 olduğundan H0 reddedilir. Yani random forest ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark vardır.

Doğrusal Regresyon Modeli ile Eksik Gözlemleri Doldurma
mice kütüphanesin de yer alan mice fonksiyonu içinde yer alan method komutunu "norm.boot" seçtiğimiz de norm.boot methodu bootstrap kullanarak doğrusal regersyon yapar.

data2 <- diamonds2[, c("carat", "price","table","lenght","depth","width","depth_1")]
imp1 <- mice(data2, method = "norm.boot", m = 1, maxit = 1,
            seed = 1, print = FALSE)
imp_Data1 = mice::complete(imp1) # veri seti haline getiren komut
Doğrusal regresyon algoritması ile imputed edilmiş veri ile orjinal verimizin ortalama, standart sapma ve varyans değerlerini karşılaştıralım.

data.frame(  actual_carat_mean = c(mean(diamonds$carat), sd(diamonds$carat),var(diamonds$carat)) 
           , imputed_carat_mean = c(mean(imp_Data1$carat), sd(imp_Data1$carat),var(imp_Data1$carat))
           , actual_price_mean = c(mean(diamonds$price), sd(diamonds$price),var(diamonds$price)) 
           , imputed_price_mean = c(mean(imp_Data1$price), sd(imp_Data1$price),var(imp_Data1$price))
           , actual_table_mean = c(mean(diamonds$table), sd(diamonds$table),var(diamonds$table)) 
           , imputed_table_mean = c(mean(imp_Data1$table), sd(imp_Data1$table),var(imp_Data1$table))
           , actual_lenght_mean = c(mean(diamonds$lenght), sd(diamonds$lenght),var(diamonds$lenght)) 
           , imputed_lenght_mean = c(mean(imp_Data1$lenght), sd(imp_Data1$lenght),var(imp_Data1$lenght))
           , actual_depth_mean = c(mean(diamonds$depth), sd(diamonds$depth),var(diamonds$depth)) 
           , imputed_depth_mean = c(mean(imp_Data1$depth), sd(imp_Data1$depth),var(imp_Data1$depth))
           , actual_width_mean = c(mean(diamonds$width), sd(diamonds$width),var(diamonds$width)) 
           , imputed_width_mean = c(mean(imp_Data1$width), sd(imp_Data1$width),var(imp_Data1$width))
           , actual_depth_1_mean = c(mean(diamonds$depth_1), sd(diamonds$depth_1),var(diamonds$depth_1)) 
           , imputed_depth_1_mean = c(mean(imp_Data1$depth_1), sd(imp_Data1$depth_1),var(imp_Data1$depth_1))
           , row.names = c("mean", "sd","varyans"))
Doğrusal regresyon algoritması ile imputed edilmiş veri ile orjinal verimizin yoğunluk grafikleri ile dağılımlarını karşılaştıralım.

par(mfrow=c(2,2))
plot(density(diamonds$carat), main = "Actual caret", type="l", col="red")
plot(density(imp_Data1$carat), main = "Imputed caret", type="l", col="red")
plot(density(diamonds$price), main = "Actual price", type="l", col="red")
plot(density(imp_Data1$price), main = "Imputed price", type="l", col="red")
Orijinal veri deki carat ve price değişkenleri ile doğrusal regresyon yöntemi ile imputed edilen verideki carat ve price değişkenleri dağılımları farklı çıkmıştır.

par(mfrow=c(3,2))
plot(density(diamonds$table), main = "Actual table", type="l", col="red")
plot(density(imp_Data1$table), main = "Imputed table", type="l", col="red")
plot(density(diamonds$lenght), main = "Actual length", type="l", col="red")
plot(density(imp_Data1$lenght), main = "Imputed length", type="l", col="red")
plot(density(diamonds$depth), main = "Actual depth", type="l", col="red")
plot(density(imp_Data1$depth), main = "Imputed depth", type="l", col="red")
Orijinal veri deki table, lenght ve depth değişkenleri ile regresyon yöntemi ile imputed edilen verideki table, lenght ve depth değişkenleri dağılımları aynı çıkmıştır.

par(mfrow=c(2,2))
plot(density(diamonds$width), main = "Actual width", type="l", col="red")
plot(density(imp_Data1$width), main = "Imputed width", type="l", col="red")
plot(density(diamonds$depth_1), main = "Actual depth_1", type="l", col="red")
plot(density(imp_Data1$depth_1), main = "Imputed depth_1", type="l", col="red")
Orijinal veri deki width ve depth_ değişkenleri ile regresyon yöntemi ile imputed edilen verideki width ve depth_1 değişkenleri dağılımları aynı çıkmıştır.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= diamonds$price, y =imp_Data1$price, conf.level = 0.95)
p-value < 0,05 olduğundan H0 reddedilir. Yani doğrusal regresyon ile doldurulan veri ve orjinal veri setimiz arasında önemli bir fark vardır.

Bayes Sınıflandırma Yöntemi ile Eksik Gözlemleri Doldurma
Bayes imputede yöntemi sadece sınıflandırma modellerinde çalıştığı için yeni veri seti alarak imputed yönteminin gerçekleştirdik.

Bağımlı Değişken

Outcome: Diaybet olma ve olmama (0:diyabet olmama, 1:diyabet olma)

Açıklayıcı Değişkenler

Pregnancies: Kadınların hamilelik sayısı

Glucose : Deneklerin glikoz miktarı

BloodPressure : Deneklerin kan basıncı

SkinThickness: Deneklerin cilt kalınlığı

Insulin : Deneklerin insülin miktarı

BMI: Deneklerin vücut kitle indeksi

DiabetesPedigreeFunction: Deneklerin diyabetik soyağacı

Age: Deneklerin yaşı

**Gözlem sayısı:**768

library(readr)
diabetes <- read_csv("C:/Users/melid/OneDrive/Masaüstü/diabetes.csv")
head(diabetes)
Veri setimizle ilgili tanımlayıcı istatistikler;

summary(diabetes)
Veri setimizde sonuç değişkenin kategorik değişken olması gerekir. Numeric değişken olduğu görülmektedir.

diabetes$Outcome<-factor(diabetes$Outcome)
Veri setimiz de eksik gözlem var mı diye kontrol edelim.

apply(is.na(diabetes), 2, sum)
Veri setimiz deki değişkenler de eskik gözlem olmadığı görülmektedir.

Verimizde 200 ile 400 arasındaki gözlemleri NA yani boş gözlem atadık.

diabetes1 = diabetes
for(i in 200:400){
  diabetes1[i,] = NA
}
summary(diabetes1)
diabetes1$Outcome<-factor(diabetes1$Outcome)
diabetes1 adında oluşturduğumuz veri setinde eksik gözlemleri kontrol edelim.

apply(is.na(diabetes1), 2, sum)
Değişkenlerimizin hepsinde 201 gözlemin eksik olduğu görülür. Bu durumu grafikle inceleyelim.

library(Amelia)
missmap(diabetes1, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)
Veri setimizin yaklaşık % 26'si eksik gözlem den oluşmaktadır. Bütün değişkenlerin eksik gözlem olması gerçek hayatta karşılacağımız bir durum değildir. Ancak biz ödevimiz kayıp veri analizi olduğundan eksik gözlem ataması yaptık.

Veri setimizi train ve test olmak üzere 2 gruba ayrırız. e1071 kütphanesindeki naiveBayes fonkiyonu Bayes karar teorisini temel olarak hesaplar, eksik veriyi ve doldurma işlemine dayanır. Bayes sınıflandırmasıyla eksik gözlemler tamamlanır. Sınıflar için ayrı ayrı olasılıklar hesaplanır. ilk önce eksik gözleme sahip verimiz için her sınıf için doğru sınıflandırma oranları hesaplar. Daha sonra ise orjinal verimiz için her sınıf için doğru sınıflandırma oranları hesaplar.

library(caret)
library(e1071)#naiveBayes fonksiyonu için
test.nb.function = function(diabetes){
    # create samples
    sample = sample(nrow(diabetes) , nrow(diabetes)* 0.75)
    train = diabetes[sample,]
    test = diabetes[-sample,]
    
    # build model
    nb.model = naiveBayes(Outcome ~., data = train)
    
    # get metrics
    metrics = confusionMatrix(predict(nb.model, test), test$Outcome)
    return(metrics$overall['Accuracy'])
    
}

Eksik gözlemlerimize imputed yapan veri setimiz deki her sınıf için doğru sınıflandırma oranları hesapladık.

actual.nb.results  = NULL
for(i in 1:100) {
    actual.nb.results[i] = test.nb.function(diabetes1)
}
head(actual.nb.results)
Orjinal veri setimiz deki her sınıf için doğru sınıflandırma oranları;

imputed.nb.results  = NULL
for(i in 1:100) {
    imputed.nb.results[i] = test.nb.function(diabetes)
}
head(data.frame(Actual = actual.nb.results, Imputed = imputed.nb.results))
Orjinal veri setimiz ile imputasyon yapılan veri setinin arasında fark olup olmadığını tespit etmek için t-test uyguladık.

H0: İki veri arasında anlamlı bir fark yoktur.

H1: İki veri arasında anlamlı bir fark vardır.

t.test(x= actual.nb.results, y = imputed.nb.results, conf.level = 0.95)
t-testinin sonuçuna göre %95 güven aralığı sonuçuna göre p-değerinin 0.05'ten küçük olduğunu görmekteyiz. Yani H0 reddedelir gerçek veri ile imputed edilen verinin arasında önemli bir fark vardır sonucuna ulaşırız.
