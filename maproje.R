
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

#Kart gruplarý arasýndaki hiyeraþiyi görebiliriz. CC en çok kullanýlan kart grubu

ggplot(satis, aes(x = Kart_Grubu, y = Yas)) +
geom_jitter()

#DD ve FF kart gruplarýný gençlerin kullandýðýný görürken kalan gruplarýn daha yaþlý insanlar tarafýndan kullanýldýðýný görüyoruz

ggplot(aes(x=Kart_Grubu, y=Uyelik_Tipi),data=satis) +
  geom_jitter(alpha=1/20, color='orange') 

# Kartlar arasýndan cc dd kart gruplarý yeni ve yneileme kategorisinde diðer kartlara göre çok daha önde





#get basic summary information on alim

head(alim)
names(alim)
summary(alim)

lenght(alim)
str(alim)
colnames(alim)

by(alim$Kart.Grubu, alim$Etkinlik.Adi, summary)
# kart grubu ile etkinlik arasýndaki iliþkiye bakaRAK HANGÝ ETKÝNLÝKLERDE HANGÝ KART GRUPLARI SIK KULLANILIYOR ONLARA BAKIYORUZ




NetSatis <- cor.test(alim$Toplam_Bilet_Fiyati, alim$Net_Satis_Geliri,
                                    method = "pearson")
Netindirim <- cor.test(alim$Toplam_Bilet_Fiyati, alim$Indirim_Kaybi, method= "pearson")

NetSatis
Netindirim

#Bilet fiyatlarý ile indirim ve net gelir arasýndaki correlation a baktýk pearson method u ile
