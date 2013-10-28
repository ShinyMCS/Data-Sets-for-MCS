#Roy's LME model
#BA99 Data
#################################################################
# Overview of Program
# 1. Entering original data set
# 2. reconfiguring data
# 3. preliminary investigation of J and S data
# 4. Subsetting to exclude R method from data
# 5. Trial LME models
###################################################################
source("Bland-Altman-blood-pressure-data.R")

library(nlme)

blood = groupedData( BP ~ method | subject ,
        data = data.frame( BP = c(Blood), subject = c(row(Blood)),
                method = rep(c("J","R","S"), rep(nrow(Blood)*3, 3)),
                obs = rep(rep(c(1:3), rep(nrow(Blood), 3)), 3) ),
        labels = list(BP = "Systolic Blood Pressure", method = "Measurement Device"),
        order.groups = FALSE )
        


#####################################################################
# consider J and S groups only:
J.sd = c(with(subset(blood, subset = method == "J"), by(BP, subject, sd)))
S.sd = c(with(subset(blood, subset = method == "S"), by(BP, subject, sd)))
min(J.sd) ; max(J.sd)
min(S.sd) ; max(S.sd)
plot(J.sd, S.sd)

######################################################################
# make a data frame containing J and S groups only:
dat = subset(blood, subset = method != "R")
#####################################################################
# lines plot of cell means:
with(dat, interaction.plot(method, subject, BP, legend = FALSE))

######################################################################
fit0 = lme( BP ~ method, data = dat, random = ~1)
fit1 = lme( BP ~ method, data = dat, random = ~1 | subject )
fit2 = update(fit1, random = ~1 | subject/method )
fit3 = update(fit1, random = ~ 1|method )
fit4 = lme( BP ~ method, data = dat, random = pdSymm(~method) )
anova(fit1, fit2, fit3, fit4)
#######################################################################
summary(fit1)
summary(fit2)














################################################################

