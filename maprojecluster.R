

## Clusterdata.csv adýnda bir data hazýraldýk. Bu datada kart idleri ile yaþlarý eþleþtirdik ve onlara ait net satýþ gelirlerini belirledik.
## Bu sayede daha koaly birt þekilde clusterlarý yaratabileceðiz. Excelde yaptýðýmýz iþlemler raproda detaylýca anlatýlacak.
clus <- read.csv("C:/Users/Serdar/Desktop/MA _Proje/clusterdata.csv", header=TRUE, sep=";")
head(clus)
library(ggplot2)

## Plotumuzu yas ve net gelir arasýnda yaptýk çünkü hangi yaþlarýn bize ne kadar getiri geitreceði ilk sorularýmýzdan biriydi.
ggplot(clus, aes(yas, netgelir)) + geom_jitter()

##Clusterlarýn random bir noktaya yerleþebilmesi için
set.seed(200)

## 10 iterasyonda elbow'u bulacaðýmýza inandýk :D
k.max <- 10

## Elbow metod kullanýlarak optimal cluster saysýnýn 5 olduðunu bulduk.
Clusterz <- sapply(1:k.max,function(k){kmeans(clus[,2:3],k,nstart = 20,iter.max = 20)$tot.withinss})
Clusterz
## Elbowumuzu görselleþtirdik
plot(1:k.max,Clusterz, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")

##Kmean metodu ile clusterlarýmýzý oluþturduk
icluster <- kmeans(clus[,2:3],5,nstart = 20)
icluster

## Renk vektörü
vcol <- c("blue","green","purple","red","yellow")

## Ve plotladýk
ggplot(clus, aes(yas, netgelir, color = vcol[icluster$cluster])) + geom_jitter()
## Plotlama ve clusterlama sonucunda yaþ aralýðýnýn kart gelirleri için çok güçlü bir gösterge olucaðýnýn kanaatinde deðiliz. 
## Þu an elimizdeki clusterlarý elimizdeki kart gruplarý ile eþleyip yaþ ve net gelire ait bir plot elde edebiliriz
## Bunu siz iksv ile konuþtuktan sorna gelicek güncellemelere göre yapabilirsek çok daha iyi olucaðýný düþünüyoruz.


