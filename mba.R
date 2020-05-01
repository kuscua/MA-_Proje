library(stringr)
Events <- read.table("https://raw.githubusercontent.com/kuscua/MA-_Proje/master/mba_MA_proje.csv", sep=";")
head(Events)
transactions <- str_split_fixed(Events$V1, ",", n = Inf)
head(transactions[,1:10])#limited to 4 columns since from the fifth column there are no items
write.csv(transactions, file = "transactions.csv", row.names = F)

library(arules)
Events <- read.transactions("transactions.csv", format = "basket", sep = ",", skip=0)

dim(Events)
cat("Number of baskets:", length(Events))
cat("Number of unique items:", sum(size(Events)))
head(itemInfo(Events))
ItemSetList <- Events@itemInfo
ItemSetList
inspect(Events[1:10])
summary(Events)

itemFrequency(Events, type = "relative")
library(arulesViz)
itemFrequencyPlot(Events, topN=11, type="relative", main="Items Frequency", cex.names=0.8) 
head(sort(itemFrequency(Events), decreasing=FALSE), n=10)
hist(size(Events), breaks = 0:40, xaxt="n", ylim=c(0,2500), 
     main = "Number of items in particular baskets", xlab = "Items")
axis(1, at=seq(0,40,by=5), cex.axis=0.8)
cat("The biggest basket consists of", ncol(transactions), "events.")

rules <- apriori(Events, parameter = list(supp = 0.01, conf = 0.25))
options(digits=2)
inspect(rules[1:25])
rules<-sort(rules, by="confidence", decreasing=TRUE)
rules

subset.rules <- which(colSums(is.subset(rules, rules)) > 1) 
length(subset.rules)
subset.rules. <- rules[-subset.rules]
subset.matrix <- is.subset(rules, rules)

rules<-apriori(data=Events, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="F"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
lhs
rules<-apriori(data=Events, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="F"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:8])
library(arulesViz)
plot(rules,method="graph",enginer="interactive",shading=NA)
