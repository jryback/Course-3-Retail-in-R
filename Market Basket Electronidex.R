#installing packages
install.packages("arules")
install.packages("arulesViz")
install.packages('jquerylib')
install.packages('cachem')
install.packages('lazyeval')

#read packages in library
library(caret)
library(ggplot2)
library(tidyverse)
library(arules)
library(arulesViz)
library(jquerylib)
library(cachem)
library(lazyeval)

#Market Basket Analysis - finding products bought together frequently
  #load data through read.transactions(), NOT read.csv()
?read.transactions
trans <- read.transactions('MarketBasket/ElectronidexTransactions2017.csv', 
                           format = 'basket', rm.duplicates = TRUE, sep = ',')
trans

#viewing the dataset
inspect(trans)
length(trans)
size(trans)
LIST(trans)
itemLabels(trans)
summary(trans)
#why all the commas? because it's a csv?
  #combine all the same things with lots of commas
    #how?
  #Note: Fixed. Problem was with read.transactions() 'sep' argument. Determines how items are separated. Default is space when it needed to be comma

itemFrequency(trans)
?itemFrequencyPlot
itemFrequencyPlot(trans, type='absolute', topN=20)
#some weird values - 'and', '2'? how to clean itemMatrix?
  #fixed with 'sep' argument - read.transactions()
itemFrequencyPlot(trans, type='relative', topN=30)



#laptop and desktop computers have the highest sales - notably imacs and other apple products did well too
image(sample(trans, 100))

#support = % of transactions in the whole dataset that have that combo of items
#confidence = % of times where LHS is true- if LHS is true,  RHS is also true
#lift = like confidence, but also if RHS is true, LHS is true
rules1 <- apriori(trans, parameter=list(supp = 0.1, conf=0.8))
inspect(rules1)
summary(rules1)
#No rules. repeat with different support/confidence

#start by adjusting confidence
rules2 <- apriori(trans, parameter=list(supp = 0.1, conf=0.6))
inspect(rules2)
summary(rules2)
#no rules
rules3 <- apriori(trans, parameter=list(supp = 0.1, conf=0.4))
inspect(rules3)
summary(rules3)
#no rules
rules4 <- apriori(trans, parameter=list(supp = 0.1, conf=0.2))
inspect(rules4)
summary(rules4)
#1 rule - customers only buy an imac? Lhs is empty. .025 supp, .25 conf
  #Not quite - includes any transaction with imac
  #Fixed by minlen parameter (minlen=2(minimum length))

#adjusting support
rules5 <- apriori(trans, parameter=list(supp=.075, conf=0.8))
inspect(rules5)
summary(rules5)
#no rules
rules6 <- apriori(trans, parameter=list(supp=.05, conf=0.8))
inspect(rules6)
summary(rules6)
#no rules
rules7 <- apriori(trans, parameter=list(supp=.025, conf=0.8))
inspect(rules7)
summary(rules7)
#no rules
rules8 <- apriori(trans, parameter=list(supp=.02, conf=0.8))
inspect(rules8)
summary(rules8)
#no rules
rules8 <- apriori(trans, parameter=list(supp=.015, conf=0.8))
inspect(rules8)
summary(rules8)
#no rules
rules9 <- apriori(trans, parameter=list(supp=.01, conf=0.8))
inspect(rules9)
summary(rules9)
#no rules
rules10 <- apriori(trans, parameter=list(supp=.005, conf=0.8))
inspect(rules10)
summary(rules10)
#1 rule - if someone buys Acer Aspire, Dell Desktop, and ViewSonic Monitor, they will buy an HP laptop
  #happens 4 times
  #buying computer parts. ways to make rules based on categories of items?
    #i.e. buying a tower and monitor? how bout some keyboards and mice? work for multiple products in same category?
    #probably not in this dataset, no categorical variable.

#try with low support and confidence
rules11 <- apriori(trans, parameter=list(supp=.005, conf=0.2))
inspect(rules11)
summary(rules11)
#too many rules! 1108 - but want to look at measurements of support and confidence
?inspect(rules11)
inspect(rules11[1:100], itemsep=',', rulesep='->')
inspect(sort(rules11[1:50], by='confidence'))
inspect(sort(rules11[1:50], by='support'))
inspect(sort(rules11[1:50], by='lift'))


#median support and confidence parameters from rules 11 summary
rules12 <- apriori(trans, parameter=list(supp=0.007, conf=0.31))
inspect(rules12)
summary(rules12)
#could be good - list of 306 rules
inspect(sort(rules12[1:50], by='confidence'))
inspect(sort(rules12[1:50], by='support'))
inspect(sort(rules12[1:50], by='lift'))
inspect(sort(rules12, by='lift'))

#A higher confidence/lift is more important than support, but I want to also to raise the support a little bit
rules13 <- apriori(trans, parameter=list(supp=0.01, conf=0.34))
inspect(rules13)
summary(rules13)
#list of 114 rules
inspect(sort(rules13[1:114], by='lift'))

#adding minlen parameter
rules14 <- apriori(trans, parameter=list(supp=0.01, conf=0.34, minlen=2))
inspect(rules14)
summary(rules14)
#list of 114 rules - minlen did not change, seems more for insurance in high support, low confidence cases
inspect(sort(rules14, by='lift'))
inspect(sort(rules14, by='lift'))
?is.redundant
is.redundant(rules14, measure='confidence')
#measure=lift or confidence - 2 redundant rules identified, 
  # rule 103 = same objects but in different places(LHS, RHS)
  #rule 80 - ???
  #are they really redundant though? - cannot prove causation
is.redundant(rules14, measure='confidence', confint=TRUE)

plot(rules14, method='graph', control=list(type='items'))
plot(rules14[1:10], method='graph', control=list(type='items'))
plot(rules14, method='paracoord', control=list(type='items'))
plot(rules14[1:10], method='paracoord', control=list(type='items'))
plot(rules14, method='scatter')
plot(rules14, method='scatter', jitter=0)
plot(rules14, method='two-key plot', jitter=0)#scatterplot with hue
plot(rules14, method='matrix')
plot(rules14, method='grouped')

colors <- c('#F5A65B','#6CAE75')

itemFrequencyPlot(trans, type='absolute', topN=20, col='#F5A65B', border=NA, main='Top 20 Items Sold', ylab='Amount Sold')
plot(rules14, method='two-key plot', jitter=0), main='Confidence and Support for Rules') + 
  scale_color_manual(values=c('#F5A65B','#6CAE75'))

ruleExplorer(rules14)

save.image()
