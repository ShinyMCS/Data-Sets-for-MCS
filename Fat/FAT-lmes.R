# Analysus under Bland and Altman
# Analysis under Roy's Method
#       - Implementation of the four models
#       - Expression of the Matrices
#       - Implementation of the three hypothesis tests        
# Analysis under BXC
#       - Computation of Limits of Agreement
#################################################################
#
# Load useful packages for analysis
library(MethComp)
library(nlme)

#################################################################
data(fat)

fat <- data.frame( item=factor(fat$Id),
meth=fat$Obs,
repl=factor(fat$Rep),
y=fat$Sub )

# summary(fat)


dat=fat
fat.roy1 = lme(y ~ meth-1, data = dat,random = list(item=pdSymm(~ meth-1)), weights=varIdent(form=~1|meth),correlation = corSymm(form=~1 | item/repl), method="ML")
fat.roy2 = lme(y ~ meth-1, data = dat,random = list(item=pdCompSymm(~ meth-1)), correlation = corSymm(form=~1 | item/repl), method="ML")
fat.roy3 = lme(y ~ meth-1, data = dat,random = list(item=pdSymm(~ meth-1)),weights=varIdent(form=~1|meth), correlation = corCompSymm(form=~1 | item/repl), method="ML")
fat.roy4 = lme(y ~ meth-1, data = dat,random = list(item=pdCompSymm(~ meth-1)), correlation = corCompSymm(form=~1 | item/repl), method="ML")

getSigma(fat.roy1)
getOmega(fat.roy1)
roy.DV(fat.roy1)


#################################################################
# Analysis using BXC

fat.bxc1 = lme( y ~ meth + item, random = list( item = pdIdent( ~ meth-1 ) ), weights = varIdent( form = ~1 | meth ), data=dat)
fat.bxc2 = lme( y ~ meth + item, random=list( item = pdIdent( ~ meth-1 ), repl = ~1 ), weights = varIdent( form = ~1 | meth ), data=dat )
