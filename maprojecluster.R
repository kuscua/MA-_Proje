

## Clusterdata.csv ad�nda bir data haz�rald�k. Bu datada kart idleri ile ya�lar� e�le�tirdik ve onlara ait net sat�� gelirlerini belirledik.
## Bu sayede daha koaly birt �ekilde clusterlar� yaratabilece�iz. Excelde yapt���m�z i�lemler raproda detayl�ca anlat�lacak.
clus <- read.csv("C:/Users/Serdar/Desktop/MA _Proje/clusterdata.csv", header=TRUE, sep=";")
head(clus)
library(ggplot2)

## Plotumuzu yas ve net gelir aras�nda yapt�k ��nk� hangi ya�lar�n bize ne kadar getiri geitrece�i ilk sorular�m�zdan biriydi.
ggplot(clus, aes(yas, netgelir)) + geom_jitter()

##Clusterlar�n random bir noktaya yerle�ebilmesi i�in
set.seed(200)

## 10 iterasyonda elbow'u bulaca��m�za inand�k :D
k.max <- 10

## Elbow metod kullan�larak optimal cluster says�n�n 5 oldu�unu bulduk.
Clusterz <- sapply(1:k.max,function(k){kmeans(clus[,2:3],k,nstart = 20,iter.max = 20)$tot.withinss})
Clusterz
## Elbowumuzu g�rselle�tirdik
plot(1:k.max,Clusterz, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")

##Kmean metodu ile clusterlar�m�z� olu�turduk
icluster <- kmeans(clus[,2:3],5,nstart = 20)
icluster

## Renk vekt�r�
vcol <- c("blue","green","purple","red","yellow")

## Ve plotlad�k
ggplot(clus, aes(yas, netgelir, color = vcol[icluster$cluster])) + geom_jitter()
## Plotlama ve clusterlama sonucunda ya� aral���n�n kart gelirleri i�in �ok g��l� bir g�sterge oluca��n�n kanaatinde de�iliz. 
## �u an elimizdeki clusterlar� elimizdeki kart gruplar� ile e�leyip ya� ve net gelire ait bir plot elde edebiliriz
## Bunu siz iksv ile konu�tuktan sorna gelicek g�ncellemelere g�re yapabilirsek �ok daha iyi oluca��n� d���n�yoruz.


