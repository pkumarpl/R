library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

retail <- read_excel('Online_retail.xlsx')
retail <- retail[complete.cases(retail), ]
retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)

#What time do people often purchase online?
retail$Time <- as.factor(retail$Time)
a %>%
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")

#distribution of item person
detach("package:plyr", unload=TRUE)
retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))
#check distrubtion
b <- retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity))
descdist(b$n_items, discrete = FALSE)


#Top 10 best sellers
tmp <- retail %>%
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

head(tmp, 10)%>%
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

#creating rules
retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))
#We only need item transactions, so remove customerID and Date columns.
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")
#write in csv
write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

#Let’s have a closer look at how many transactions we have and what they are
tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)
itemFrequencyPlot(tr, topN=20, type='absolute')

#create rule
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

#inspect rules
inspect(rules[1:10])

#plot rule
topRules <- rules[1:10]
plot(topRules)

#graph
plot(topRules, method="graph")

#group plot
plot(topRules, method = "grouped")

#matrix method
plot(head(sort(rules),20), method = "matrix", measure = c("lift", "confidence"), control=list(reorder = T))

#double decker
samplerule <- head(sort(rules, by = "lift"), 1)
inspect(samplerule)
plot(samplerule, method = "doubledecker", data = tr)  


im <- interestMeasure(head(sort(rules),20), c("coverage", "oddsRatio", "leverage", "hyperConfidence", "chiSquared"), transactions = retail)
head(im)
