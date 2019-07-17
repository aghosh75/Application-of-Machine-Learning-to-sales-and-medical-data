# Associaton rules are used to figure what items are purchased together.

#This dataset is for illustrative purposes only based on customer purcases of nine different items. 

library(readxl)
AssociationRules <- read_excel("~/LearningR/AssociationRules.xlsx")
View(AssociationRules)
attach(AssociationRules)
str(AssociationRules)
summary(AssociationRules)

# We need the following library for Asociation rules
library(arules)

# For association rules we need to have all vraibles as factor variables.
AssociationRules$Bag <- as.factor(AssociationRules$Bag)
AssociationRules$Bat <- as.factor(AssociationRules$Bat)
AssociationRules$Milk <- as.factor(AssociationRules$Milk)
AssociationRules$Sugar <- as.factor(AssociationRules$Sugar)
AssociationRules$Brushes <- as.factor(AssociationRules$Brushes)
AssociationRules$Concealers <- as.factor(AssociationRules$Concealers)
AssociationRules$Pencils <- as.factor(AssociationRules$Pencils)
AssociationRules$Pens <- as.factor(AssociationRules$Pens)
AssociationRules$Lipsticks <- as.factor(AssociationRules$Lipsticks)

#Finding association rules
rules <- apriori(AssociationRules) # We create some rules here.

# We get the summary of the rules and summary of quality measures
summary(rules)


#Reduce to smaller number of rules. We specify the paramerers with minimum and maximum length. we are restricting the rules to 2 and 3 and the support value to 70%.
rules <- apriori(AssociationRules, parameter = list(minlen=2, maxlen = 3, supp = 0.7))
inspect(rules)
summary(AssociationRules)

# We are interested in knowing what customers are purchasing and not what they are not purchasing
#Finding interesting rules -1
rules <- apriori(AssociationRules, parameter = list(minlen=2, maxlen = 3, supp = 0.7), appearance = list(rhs=c("Bag=Yes"), default="lhs"))
inspect(rules)

#Visualization rules
library(arulesViz)
plot(rules)
plot(rules, method = "grouped")
plot(rules, method = "graph", control = list(type ="items"))

#Finding interesting rules -2
rules <- apriori(AssociationRules, parameter = list(minlen=2, maxlen = 5, supp = 0.1), appearance = list(rhs=c("Bag=Yes"), lhs = c("Bat=Yes", "Milk=Yes", "Sugar=Yes", "Brushes=Yes", "Concealers=Yes", "Pencils=Yes", "Pens=Yes", "Lipsticks=Yes"),default ="none")) 
quality(rules)<-round(quality(rules),digits =3) #This rounds them to 3-digit values.
inspect(rules)                 

#Finding redudant rules
subset.matrix<- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=1
which(redundant)

#Removing redundant rules and sort them using lift.
rules.pruned<-rules[redundant]
rules.pruned<-sort(rules.pruned, by = "lift")
inspect(rules.pruned)
