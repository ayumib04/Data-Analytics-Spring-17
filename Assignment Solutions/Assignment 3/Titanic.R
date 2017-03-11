TitanicData <- read.csv("Titanic_train.csv", stringsAsFactors = FALSE)
head(TitanicData)
library(ggplot2)
#library(gam)
summary(TitanicData)
TitanicData$Survived <- as.factor(TitanicData$Survived)

## VARIABLE 'PCLASS'
# 3rd class represents more than half of the total population (55.11%)
#summary(Pclass)
#ggplot(TitanicData, aes(x=Pclass, fill = Sex)) + geom_histogram()
# survival rate is much higher on 1st class passenger and decrease consistently among each
# class. Surely this is an important covariate!
#prop.table(table(Pclass, Survived), 1)
#ggplot(TitanicData, aes(x = Pclass, fill = Survived)) + geom_bar()
## VARIABLE 'SEX'
# males are more numerous than females
#summary(Sex)
#ggplot(TitanicData, aes(x = Sex, fill = Sex)) + geom_bar()
# famales have a much higher likelihood to survive (.74 vs .19)
#prop.table(table(Sex, Survived), 1)
#ggplot(TitanicData, aes(x = Sex, fill = Survived)) + geom_bar()
#str(TitanicData)
#head(TitanicData)

##BOXPLOT##
# filtering by 'Survived' we can see both groups have approximately the same median, but 
# the shape is different. 'Non survived' is right skewed, whereas 'Survived' is left skewed.
gbox <- ggplot(TitanicData, aes(x = Survived, y = Age)) + geom_boxplot()
gbox
##HISTOGRAM##
g <- ggplot(TitanicData, aes(Age))
g + geom_histogram(aes(fill = Sex))
g + geom_histogram(aes(fill = Embarked))
g + geom_histogram(aes(color = Pclass))
##FACET##
gbox + facet_grid(~Sex+Embarked+Pclass)
#gbox + facet_grid(~SibSp) #how many siblings does one have and what's its relatio to survival
##VIOLIN##
ggplot(TitanicData, aes(x = Age, y = Fare, colour = Survived)) + geom_violin()
ggplot(TitanicData, aes(x = Survived, y = Fare, colour = Sex)) + geom_violin()
ggplot(TitanicData, aes(x = Survived, y = Fare, colour = Embarked)) + geom_violin()
##FINALPLOT##
ggplot(TitanicData, aes(x = Pclass, fill = Survived)) + geom_bar()
