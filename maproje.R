
library(dplyr)
library(ggplot2)
alim <- read.csv("C:/Users/Serdar/Desktop/MA _Proje/alim.csv", header=TRUE, sep=";")
satis <- read.csv("C:/Users/Serdar/Desktop/MA _Proje/satis.csv", header=TRUE, sep=";")
#


#get basic summary information on satis
head(satis)
names(satis)
summary(satis)

length(satis)
str(satis)
colnames(satis)


by(satis$Kart_Grubu, satis$Etkinlik_Adi, summary)
by(satis$Net_Satis_Geliri, satis$Uyelik_Tipi, summary)

qplot(x=satis$Kart_Grubu , data=satis)

#Kart gruplar� aras�ndaki hiyera�iyi g�rebiliriz. CC en �ok kullan�lan kart grubu

ggplot(satis, aes(x = Kart_Grubu, y = Yas)) +
geom_jitter()

#DD ve FF kart gruplar�n� gen�lerin kulland���n� g�r�rken kalan gruplar�n daha ya�l� insanlar taraf�ndan kullan�ld���n� g�r�yoruz

ggplot(aes(x=Kart_Grubu, y=Uyelik_Tipi),data=satis) +
  geom_jitter(alpha=1/20, color='orange') 

# Kartlar aras�ndan cc dd kart gruplar� yeni ve yneileme kategorisinde di�er kartlara g�re �ok daha �nde





#get basic summary information on alim

head(alim)
names(alim)
summary(alim)

lenght(alim)
str(alim)
colnames(alim)

by(alim$Kart.Grubu, alim$Etkinlik.Adi, summary)
# kart grubu ile etkinlik aras�ndaki ili�kiye bakaRAK HANG� ETK�NL�KLERDE HANG� KART GRUPLARI SIK KULLANILIYOR ONLARA BAKIYORUZ




NetSatis <- cor.test(alim$Toplam_Bilet_Fiyati, alim$Net_Satis_Geliri,
                                    method = "pearson")
Netindirim <- cor.test(alim$Toplam_Bilet_Fiyati, alim$Indirim_Kaybi, method= "pearson")

NetSatis
Netindirim

#Bilet fiyatlar� ile indirim ve net gelir aras�ndaki correlation a bakt�k pearson method u ile
